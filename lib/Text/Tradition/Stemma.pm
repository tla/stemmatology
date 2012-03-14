package Text::Tradition::Stemma;

use Bio::Phylo::IO;
use Encode qw( decode_utf8 );
use File::Temp;
use Graph;
use Graph::Reader::Dot;
use IPC::Run qw/ run binary /;
use Text::Tradition::Error;
use Text::Tradition::StemmaUtil qw/ character_input phylip_pars parse_newick /;
use XML::LibXML;
use Moose;

=head1 NAME

Text::Tradition::Stemma - a representation of a I<stemma codicum> for a Text::Tradition

=head1 SYNOPSIS

  use Text::Tradition;
  my $t = Text::Tradition->new( 
    'name' => 'this is a text',
    'input' => 'TEI',
    'file' => '/path/to/tei_parallel_seg_file.xml' );

  my $s = $tradition->add_stemma( dotfile => '/path/to/stemma.dot' );
    
=head1 DESCRIPTION

Text::Tradition is a library for representation and analysis of collated
texts, particularly medieval ones.  The Collation is the central feature of
a Tradition, where the text, its sequence of readings, and its relationships
between readings are actually kept.

=head1 DOT SYNTAX

The easiest way to define a stemma (which is a directed acyclic graph, denoting 
the scholar's hypothesis concerning which text(s) were copied from which other(s)) 
is to use a special form of the 'dot' syntax of GraphViz.  

Each stemma opens with the line

 digraph Stemma {
 
and continues with a list of all manuscript witnesses in the stemma, whether
extant witnesses or missing archetypes or hyparchetypes.  Each of these is
listed by its sigil on its own line, e.g.:

  alpha [ class=hypothetical ]
  1 [ class=hypothetical,label=* ]
  Ms4 [ class=extant ]
  
Extant witnesses are listed with class=extant; missing or postulated witnesses
are listed with class=hypothetical.  Anonymous hyparchetypes must be given a 
unique name or number, but can be represented as anonymous with the addition 
of 'label=*' to their lines.  Greek letters or other special characters may be
used as names, but they must always be wrapped in double quotes.

Links between manuscripts are then listed with arrow notation, as below. These 
lines show the direction of copying, one step at a time, for the entire stemma.

  alpha -> 1
  1 -> Ms4
  
The final line in the definition should be the closing brace:

 }
  
Thus for a set of extant manuscripts A, B, and C, where A and B were copied 
from the archetype O and C was copied from B, the definition would be:

 digraph Stemma {
     O [ class=hypothetical]
     A [ class=extant ]
     B [ class=extant ]
     C [ class=extant ]
     O -> A
     O -> B
     B -> C
 }

=head1 CONSTRUCTOR

=head2 new

The constructor.  This should generally be called from Text::Tradition, but
if called directly it takes the following options:

=over

=item * collation - The collation with which the stemma is associated.

=item * dot - A filehandle open to a DOT representation of the stemma graph.

=back

=cut

has collation => (
    is => 'ro',
    isa => 'Text::Tradition::Collation',
    required => 1,
    weak_ref => 1,
    );  

has graph => (
    is => 'rw',
    isa => 'Graph',
    predicate => 'has_graph',
    );
        
sub BUILD {
    my( $self, $args ) = @_;
    # If we have been handed a dotfile, initialize it into a graph.
    if( exists $args->{'dot'} ) {
        $self->_graph_from_dot( $args->{'dot'} );
    }
}

sub _graph_from_dot {
	my( $self, $dotfh ) = @_;
 	my $reader = Graph::Reader::Dot->new();
	my $graph = $reader->read_graph( $dotfh );
	if( $graph ) {
		$self->graph( $graph );
		# Go through the nodes and set any non-hypothetical node to extant.
		foreach my $v ( $self->graph->vertices ) {
			$self->graph->set_vertex_attribute( $v, 'class', 'extant' )
				unless $self->graph->has_vertex_attribute( $v, 'class' );
		}
	} else {
		throw( "Failed to parse dot in $dotfh" );
	}
}

=head1 METHODS

=head2 as_dot( \%options )

Returns a normal dot representation of the stemma layout, suitable for rendering
with GraphViz.  Options include:

=over

=item * graph - A hashref of global graph options.

=item * node - A hashref of global node options.

=item * edge - A hashref of global edge options.

=back

See the GraphViz documentation for the list of available options.

=cut

sub as_dot {
    my( $self, $opts ) = @_;
    
    # Get default and specified options
    my %graphopts = (
    	# 'ratio' => 1,
    );
    my %nodeopts = (
		'fontsize' => 11,
		'style' => 'filled',
		'fillcolor' => 'white',
		'color' => 'white',
		'shape' => 'ellipse',	# Shape for the extant nodes
	);
	my %edgeopts = (
		'arrowhead' => 'none',
	);
	@graphopts{ keys %{$opts->{'graph'}} } = values %{$opts->{'graph'}} 
		if $opts->{'graph'};
	@nodeopts{ keys %{$opts->{'node'}} } = values %{$opts->{'node'}} 
		if $opts->{'node'};
	@edgeopts{ keys %{$opts->{'edge'}} } = values %{$opts->{'edge'}} 
		if $opts->{'edge'};

	my @dotlines;
	push( @dotlines, 'digraph stemma {' );
	## Print out the global attributes
	push( @dotlines, _make_dotline( 'graph', %graphopts ) ) if keys %graphopts;
	push( @dotlines, _make_dotline( 'edge', %edgeopts ) ) if keys %edgeopts;
	push( @dotlines, _make_dotline( 'node', %nodeopts ) ) if keys %nodeopts;

	# Add each of the nodes.
    foreach my $n ( $self->graph->vertices ) {
        if( $self->graph->has_vertex_attribute( $n, 'label' ) ) {
        	my $ltext = $self->graph->get_vertex_attribute( $n, 'label' );
        	push( @dotlines, _make_dotline( $n, 'label' => $ltext ) );
        } else {
        	# Use the default display settings.
            push( @dotlines, "  $n;" );
        }
    }
    # Add each of our edges.
    foreach my $e ( $self->graph->edges ) {
    	my( $from, $to ) = @$e;
    	push( @dotlines, "  $from -> $to;" );
    }
    push( @dotlines, '}' );
    
    return join( "\n", @dotlines );
}

=head2 editable( $linesep )

Returns a version of the graph rendered in our definition format.  The
$linesep argument defaults to newline; set it to the empty string or to
a space if the result is to be sent via JSON.

=cut

sub editable {
	my $self = shift;
	my $join = shift || "\n";
	my @dotlines;
	push( @dotlines, 'digraph stemma {' );
	my @real; # A cheap sort
    foreach my $n ( sort $self->graph->vertices ) {
    	my $c = $self->graph->get_vertex_attribute( $n, 'class' );
    	$c = 'extant' unless $c;
    	if( $c eq 'extant' ) {
    		push( @real, $n );
    	} else {
			push( @dotlines, _make_dotline( $n, 'class' => $c ) );
		}
    }
	# Now do the real ones
	foreach my $n ( @real ) {
		push( @dotlines, _make_dotline( $n, 'class' => 'extant' ) );
	}
	foreach my $e ( sort _by_vertex $self->graph->edges ) {
		my( $from, $to ) = @$e;
		push( @dotlines, "  $from -> $to;" );
	}
    push( @dotlines, '}' );
    return join( $join, @dotlines );
}

sub _make_dotline {
	my( $obj, %attr ) = @_;
	my @pairs;
	foreach my $k ( keys %attr ) {
		my $v = $attr{$k};
		$v =~ s/\"/\\\"/g;
		push( @pairs, "$k=\"$v\"" );
	}
	return sprintf( "  %s [ %s ];", $obj, join( ', ', @pairs ) );
}
	
sub _by_vertex {
	return $a->[0].$a->[1] cmp $b->[0].$b->[1];
}

=head2 as_svg

Returns an SVG representation of the graph, calling as_dot first.

=cut

sub as_svg {
    my( $self, $opts ) = @_;
    my $dot = $self->as_dot( $opts );
    my @cmd = qw/dot -Tsvg/;
    my $svg;
    my $dotfile = File::Temp->new();
    ## TODO REMOVE
    # $dotfile->unlink_on_destroy(0);
    binmode $dotfile, ':utf8';
    print $dotfile $dot;
    push( @cmd, $dotfile->filename );
    run( \@cmd, ">", binary(), \$svg );
    # HACK: Parse the SVG and change the dimensions.
    # Convert width from pt to px, and remove height to allow scaling.
    my $parser = XML::LibXML->new();
    my $svgdoc = $parser->parse_string( decode_utf8( $svg ) );
	my $dval = $svgdoc->documentElement->getAttribute('width');
	$dval =~ s/pt/px/;
	$svgdoc->documentElement->setAttribute( 'width', $dval );
	$svgdoc->documentElement->removeAttribute('height');
    # Return the result
    return decode_utf8( $svgdoc->toString );
}

=head2 witnesses

Returns a list of the extant witnesses represented in the stemma.

=cut

sub witnesses {
    my $self = shift;
    my @wits = grep { $self->graph->get_vertex_attribute( $_, 'class' ) eq 'extant' }
        $self->graph->vertices;
    return @wits;
}

sub hypotheticals {
    my $self = shift;
    my @wits = grep 
    	{ $self->graph->get_vertex_attribute( $_, 'class' ) eq 'hypothetical' }
        $self->graph->vertices;
    return @wits;
}

sub throw {
	Text::Tradition::Error->throw( 
		'ident' => 'Stemma error',
		'message' => $_[0],
		);
}


no Moose;
__PACKAGE__->meta->make_immutable;
    
1;

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews E<lt>aurum@cpan.orgE<gt>
