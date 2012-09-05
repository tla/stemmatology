package Text::Tradition::Stemma;

use Bio::Phylo::IO;
use Encode qw( decode_utf8 );
use File::Temp;
use Graph;
use Graph::Reader::Dot;
use IPC::Run qw/ run binary /;
use Text::Tradition::Error;
use Text::Tradition::StemmaUtil qw/ character_input phylip_pars parse_newick /;
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

=begin testing

use Text::Tradition::Collation;
use TryCatch;

use_ok( 'Text::Tradition::Stemma' );

# Placeholder collation to use in tests
my $c = Text::Tradition::Collation->new();

# Try to create a bad graph
my $baddotfh;
open( $baddotfh, 't/data/besoin_bad.dot' ) or die "Could not open test dotfile";
try {
	my $stemma = Text::Tradition::Stemma->new( collation => $c, dot => $baddotfh );
	ok( 0, "Created broken stemma from dotfile with syntax error" );
} catch( Text::Tradition::Error $e ) {
	like( $e->message, qr/^Error trying to parse/, "Syntax error in dot threw exception" );
}

# Create a good graph
my $dotfh;
open( $dotfh, 't/data/florilegium.dot' ) or die "Could not open test dotfile";
binmode( $dotfh, ':utf8' );
my $stemma = Text::Tradition::Stemma->new( collation => $c, dot => $dotfh );
is( ref( $stemma ), 'Text::Tradition::Stemma', "Created stemma from good dotfile" );
is( scalar $stemma->witnesses, 13, "Found correct number of extant witnesses" );
is( scalar $stemma->hypotheticals, 8, "Found correct number of extant hypotheticals" );
my $found_unicode_sigil;
foreach my $h ( $stemma->hypotheticals ) {
	$found_unicode_sigil = 1 if $h eq "\x{3b1}";
}
ok( $found_unicode_sigil, "Found a correctly encoded Unicode sigil" );

=end testing

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
 	# Redirect STDOUT in order to trap any error messages - syntax errors
 	# are evidently not fatal.
 	my $reader_out;
 	my $saved_stderr;
 	open $saved_stderr, ">&STDOUT";
 	close STDOUT;
 	open STDOUT, ">", \$reader_out;
	my $graph = $reader->read_graph( $dotfh );
	close STDOUT;
	open STDOUT, ">", \$saved_stderr;
	if( $reader_out && $reader_out =~ /error/s ) {
		throw( "Error trying to parse dot: $reader_out" );
	} elsif( !$graph ) {
		throw( "Failed to create graph from dot" );
	}
	$self->graph( $graph );
	# Go through the nodes and set any non-hypothetical node to extant.
	foreach my $v ( $self->graph->vertices ) {
		$self->graph->set_vertex_attribute( $v, 'class', 'extant' )
			unless $self->graph->has_vertex_attribute( $v, 'class' );
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
		my $extant = {};
		map { $extant->{$_} = 1 } $self->witnesses;
		$graph = $self->situation_graph( $extant, $opts->{'layerwits'} );
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

=head2 alter_graph( $dotstring )

Alters the graph of this stemma according to the definition specified
in $dotstring.

=cut

sub alter_graph {
	my( $self, $dotstring ) = @_;
	my $dotfh;
	open $dotfh, '<', \$dotstring;
	binmode $dotfh, ':utf8';
	$self->_graph_from_dot( $dotfh );
}

=head2 editable( $opts )

=head2 editable_graph( $graph, $opts )

Returns a version of the graph rendered in our definition format.  The
output separates statements with a newline; set $opts->{'linesep'} to the 
empty string or to a space if the result is to be sent via JSON.

If a situational version of the stemma is required, the arguments for 
situation_graph should be passed via $opts->{'extant'} and $opts->{'layerwits'}.

=cut

sub editable {
	my( $self, $opts ) = @_;	
	my $graph = $self->graph;
	## See if we need an editable version of a situational graph.
	if( exists $opts->{'layerwits'} || exists $opts->{'extant'} ) {
		my $extant = delete $opts->{'extant'} || {};
		my $layerwits = delete $opts->{'layerwits'} || [];
		$graph = $self->situation_graph( $extant, $layerwits );
	}
	return editable_graph( $graph, $opts );
}

sub editable_graph {
	my( $graph, $opts ) = @_;

	# Create the graph
	my $join = ( $opts && exists $opts->{'linesep'} ) ? $opts->{'linesep'} : "\n";
	my @dotlines;
	push( @dotlines, 'digraph stemma {' );
	my @real; # A cheap sort
    foreach my $n ( sort $graph->vertices ) {
    	my $c = $graph->get_vertex_attribute( $n, 'class' );
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
	foreach my $e ( sort _by_vertex $graph->edges ) {
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

=head2 situation_graph( $extant, $layered )

Returns a graph which is the original stemma with all witnesses not in the
%$extant hash marked as hypothetical, and witness layers added to the graph
according to the list in @$layered.  A layered (a.c.) witness is added as a
parent of its main version, and additionally shares all other parents and
children with that version.

=cut

sub situation_graph {
	my( $self, $extant, $layerwits ) = @_;
	
	my $graph = $self->graph->copy;
	foreach my $vertex ( $graph->vertices ) {
		# Set as extant any vertex that is extant in the stemma AND 
		# exists in the $extant hash.
		my $class = 'hypothetical';
		$class = 'extant' if exists $extant->{$vertex} && $extant->{$vertex} &&
			$self->graph->get_vertex_attribute( $vertex, 'class' ) ne 'hypothetical';
		$graph->set_vertex_attribute( $vertex, 'class', $class );
	}
	
	# For each 'layered' witness in the layerwits array, add it to the graph
	# as an ancestor of the 'main' witness, and otherwise with the same parent/
	# child links as its main analogue.
	# TOOD Handle case where B is copied from A but corrected from C
	my $aclabel = $self->collation->ac_label;
	foreach my $lw ( @$layerwits ) {
		# Add the layered witness and set it with the same attributes as
		# its 'main' analogue
		throw( "Cannot add a layer to a hypothetical witness $lw" )
			unless $graph->get_vertex_attribute( $lw, 'class' ) eq 'extant';
		my $lwac = $lw . $aclabel;
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
			$graph->add_edge( $v.$aclabel, $lwac )
				if $graph->has_vertex( $v.$aclabel );
		}
		foreach my $v ( $graph->successors( $lw ) ) {
			next if $v eq $lwac; # but this shouldn't occur
			$graph->add_edge( $lwac, $v );
			$graph->add_edge( $lwac, $v.$aclabel )
				if $graph->has_vertex( $v.$aclabel );
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
    close $dotfile;
    push( @cmd, $dotfile->filename );
    run( \@cmd, ">", binary(), \$svg );
    # HACK: Parse the SVG and change the dimensions.
    # Get rid of width and height attributes to allow scaling.
    if( $opts->{'size'} ) {
    	require XML::LibXML;
		my $parser = XML::LibXML->new( load_ext_dtd => 0 );
		my $svgdoc;
		eval {
			$svgdoc = $parser->parse_string( decode_utf8( $svg ) );
		};
		throw( "Could not reparse SVG: $@" ) if $@;
    	my( $ew, $eh ) = @{$opts->{'size'}};
    	# If the graph is wider than it is tall, set width to ew and remove height.
    	# Otherwise set height to eh and remove width.
		my $width = $svgdoc->documentElement->getAttribute('width');
		my $height = $svgdoc->documentElement->getAttribute('height');
		$width =~ s/\D+//g;
		$height =~ s/\D+//g;
		my( $remove, $keep, $val, $viewbox );
		if( $width > $height ) {
			$remove = 'height';
			$keep = 'width';
			$val = $ew . 'px';
			my $vbheight = $width / $ew * $height;
			$viewbox = "0.00 0.00 $width.00" . sprintf( "%.2f", $vbheight );
		} else {
			$remove = 'width';
			$keep = 'height';
			$val = $eh . 'px';
			my $vbwidth = $height / $eh * $width;
			$viewbox = "0.00 0.00 " . sprintf( "%.2f", $vbwidth ) . " $height.00";
		}
		$svgdoc->documentElement->removeAttribute( $remove );
		$svgdoc->documentElement->setAttribute( $keep, $val );
		$svgdoc->documentElement->removeAttribute( 'viewBox' );
		$svgdoc->documentElement->setAttribute( 'viewBox', $viewbox );
		$svg = $svgdoc->toString();
	}
    # Return the result
    return decode_utf8( $svg );
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
