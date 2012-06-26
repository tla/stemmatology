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
texts, particularly medieval ones.  The Stemma is a representation of the
copying relationships between the witnesses in a Tradition, modelled with
a connected rooted directed acyclic graph (CRDAG).

=head1 DOT SYNTAX

The easiest way to define a stemma is to use a special form of the 'dot' 
syntax of GraphViz.  

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
    
	## See if we are including any a.c. witnesses in this graph.
	my $graph = $self->graph;
	if( exists $opts->{'layerwits'} ) {
		$graph = $self->extend_graph( $opts->{'layerwits'} );
	}

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
    foreach my $n ( $graph->vertices ) {
        if( $graph->has_vertex_attribute( $n, 'label' ) ) {
        	my $ltext = $graph->get_vertex_attribute( $n, 'label' );
        	push( @dotlines, _make_dotline( $n, 'label' => $ltext ) );
        } else {
        	# Use the default display settings.
        	$n = _dotquote( $n );
            push( @dotlines, "  $n;" );
        }
    }
    # Add each of our edges.
    foreach my $e ( $graph->edges ) {
    	my( $from, $to ) = map { _dotquote( $_ ) } @$e;
    	push( @dotlines, "  $from -> $to;" );
    }
    push( @dotlines, '}' );
    
    return join( "\n", @dotlines );
}

=head2 editable( $opts )

Returns a version of the graph rendered in our definition format.  The
output separates statements with a newline; set $opts->{'linesep'} to the 
empty string or to a space if the result is to be sent via JSON.

Any layer witnesses to be included should be passed via $opts->{'layerwits'}.

=cut

sub editable {
	my( $self, $opts ) = @_;
	
	## See if we are including any a.c. witnesses in this graph.
	my $graph = $self->graph;
	if( exists $opts->{'layerwits'} ) {
		$graph = $self->extend_graph( $opts->{'layerwits'} );
	}

	# Create the graph
	my $join = ( $opts && exists $opts->{'linesep'} ) ? $opts->{'linesep'} : "\n";
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
		my( $from, $to ) = map { _dotquote( $_ ) } @$e;
		push( @dotlines, "  $from -> $to;" );
	}
    push( @dotlines, '}' );
    return join( $join, @dotlines );
}

sub _make_dotline {
	my( $obj, %attr ) = @_;
	my @pairs;
	foreach my $k ( keys %attr ) {
		my $v = _dotquote( $attr{$k} );
		push( @pairs, "$k=$v" );
	}
	return sprintf( "  %s [ %s ];", _dotquote( $obj ), join( ', ', @pairs ) );
}
	
sub _dotquote {
	my( $str ) = @_;
	return $str if $str =~ /^[A-Za-z0-9]+$/;
	$str =~ s/\"/\\\"/g;
	$str = '"' . $str . '"';
	return $str;
}

sub _by_vertex {
	return $a->[0].$a->[1] cmp $b->[0].$b->[1];
}

=head2 extend_graph( $layered_witnesses )

Returns a graph which is the original stemma with witness layers added for the
list in @$layered_witnesses.  A layered (a.c.) witness is added as a parent
of its main version, and additionally shares all other parents and children with
that version.

=cut

sub extend_graph {
	my( $self, $layerwits ) = @_;
	# For each 'layered' witness in the layerwits array, add it to the graph
	# as an ancestor of the 'main' witness, and otherwise with the same parent/
	# child links as its main analogue.
	# TOOD Handle case where B is copied from A but corrected from C
	
	# Iterate through, adding a.c. witnesses
	my $actag = $self->collation->ac_label;
	my $graph = $self->graph->deep_copy;
	foreach my $lw ( @$layerwits ) {
		# Add the layered witness and set it with the same attributes as
		# its 'main' analogue
		my $lwac = $lw . $self->collation->ac_label;
		$graph->add_vertex( $lwac );
		$graph->set_vertex_attributes( $lwac,
			$graph->get_vertex_attributes( $lw ) );
			
		# Set it as ancestor to the main witness
		$graph->add_edge( $lwac, $lw );
		
		# Give it the same ancestors and descendants as the main witness has,
		# bearing in mind that those ancestors and descendants might also just
		# have had a layered witness defined.
		foreach my $v ( $graph->predecessors( $lw ) ) {
			next if $v eq $lwac; # Don't add a loop
			$graph->add_edge( $v, $lwac );
			$graph->add_edge( $v.$self->collation->ac_label, $lwac )
				if $graph->has_vertex( $v.$self->collation->ac_label );
		}
		foreach my $v ( $graph->successors( $lw ) ) {
			next if $v eq $lwac; # but this shouldn't occur
			$graph->add_edge( $lwac, $v );
			$graph->add_edge( $lwac, $v.$self->collation->ac_label )
				if $graph->has_vertex( $v.$self->collation->ac_label );
		}
	}
	return $graph;
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
    # Get rid of width and height attributes to allow scaling.
    my $parser = XML::LibXML->new();
    my $svgdoc = $parser->parse_string( decode_utf8( $svg ) );
	$svgdoc->documentElement->removeAttribute('width');
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

=head2 hypotheticals

Returns a list of the hypothetical witnesses represented in the stemma.

=cut

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
