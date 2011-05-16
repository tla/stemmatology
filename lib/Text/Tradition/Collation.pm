package Text::Tradition::Collation;

use Graph::Easy;
use IPC::Run qw( run binary );
use Module::Load;
use Text::Tradition::Collation::Reading;
use Moose;

has 'graph' => (
    is => 'ro',
    isa => 'Graph::Easy',
    handles => {
	add_reading => 'add_node',
	del_reading => 'del_node',
	add_path => 'add_edge',
	del_path => 'del_edge',
	reading => 'node',
	path => 'edge',
	readings => 'nodes',
	paths => 'edges',
    },
    default => sub { Graph::Easy->new( undirected => 0 ) },
    );
		

has 'tradition' => (
    is => 'rw',
    isa => 'Text::Tradition',
    );

has 'svg' => (
    is => 'ro',
    isa => 'Str',
    writer => '_save_svg',
    predicate => 'has_svg',
    );

has 'graphviz' => (
    is => 'ro',
    isa => 'Str',
    writer => '_save_graphviz',
    predicate => 'has_graphviz',
    );

has 'graphml' => (
    is => 'ro',
    isa => 'XML::LibXML::Document',
    writer => '_save_graphml',
    predicate => 'has_graphml',
    );

# The collation can be created two ways:
# 1. Collate a set of witnesses (with CollateX I guess) and process
#    the results as in 2.
# 2. Read a pre-prepared collation in one of a variety of formats,
#    and make the graph from that.

# The graph itself will (for now) be immutable, and the positions
# within the graph will also be immutable.  We need to calculate those
# positions upon graph construction.  The equivalences between graph
# nodes will be mutable, entirely determined by the user (or possibly
# by some semantic pre-processing provided by the user.)  So the
# constructor should just make an empty equivalences object.  The
# constructor will also need to make the witness objects, if we didn't
# come through option 1.

sub BUILD {
    my( $self, $args ) = @_;

    # Call the appropriate parser on the given data
    my @formats = grep { /^(GraphML|CSV|CTE|TEI)$/ } keys( %$args );
    my $format = shift( @formats );
    unless( $format ) {
	warn "No data given to create a graph; will initialize an empty one";
    }
    if( $format && $format =~ /^(CSV|CTE)$/ && !exists $args->{'base'} ) {
	warn "Cannot make a graph from $format without a base text";
	return;
    }

    # Initialize our graph object.
    $self->graph->use_class('node', 'Text::Tradition::Collation::Reading');
    $self->graph->set_attribute( 'node', 'shape', 'ellipse' );
    # Starting point for all texts
    my $last_node = $self->add_reading( '#START#' );

    # Now do the parsing.
    my @sigla;
    if( $format ) {
	my @parseargs;
	if( $format =~ /^(CSV|CTE)$/ ) {
	    @parseargs = ( 'base' => $args->{'base'},
		      'data' => $args->{$format},
		      'format' => $format );
	    $format = 'BaseText';
	} else {
	    @parseargs = ( $args->{ $format } ); 
	}
	my $mod = "Text::Tradition::Parser::$format";
	load( $mod );
	# TODO parse needs to return witness IDs
	@sigla = $mod->can('parse')->( $self, @parseargs );
    }

    # Do we need to initialize the witnesses?
    unless( $args->{'have_witnesses'} ) {
	# initialize Witness objects for all our witnesses
	my @witnesses;
	foreach my $sigil ( @sigla ) {
	    push( @witnesses, Text::Tradition::Witness->new( 'sigil' => $sigil ) );
	}
	$self->tradition->witnesses( \@witnesses );
    }
}

# Wrappers around some methods

sub merge_readings {
    my $self = shift;
    my $first_node = shift;
    my $second_node = shift;
    $first_node->merge_from( $second_node );
    unshift( @_, $first_node, $second_node );
    return $self->graph->merge_nodes( @_ );
}

=head2 Output method(s)

=over

=item B<as_svg>

print $graph->as_svg( $recalculate );

Returns an SVG string that represents the graph.  Uses GraphViz to do
this, because Graph::Easy doesn't cope well with long graphs. Unless
$recalculate is passed (and is a true value), the method will return a
cached copy of the SVG after the first call to the method.

=cut

sub as_svg {
    my( $self, $recalc ) = @_;
    return $self->svg if $self->has_svg;
    
    $self->_save_graphviz( $self->graph->as_graphviz() )
	unless( $self->has_graphviz && !$recalc );
    
    my @cmd = qw/dot -Tsvg/;
    my( $svg, $err );
    my $in = $self->graphviz;
    run( \@cmd, \$in, ">", binary(), \$svg );
    $self->{'svg'} = $svg;
    return $svg;
}

=item B<as_graphml>

print $graph->as_graphml( $recalculate )

Returns a GraphML representation of the collation graph, with
transposition information and position information. Unless
$recalculate is passed (and is a true value), the method will return a
cached copy of the SVG after the first call to the method.

=cut

sub as_graphml {
    my( $self, $recalc ) = @_;
    return $self->graphml if $self->has_graphml;

    # Some namespaces
    my $graphml_ns = 'http://graphml.graphdrawing.org/xmlns';
    my $xsi_ns = 'http://www.w3.org/2001/XMLSchema-instance';
    my $graphml_schema = 'http://graphml.graphdrawing.org/xmlns ' .
	'http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd';

    # Create the document and root node
    my $graphml = XML::LibXML->createDocument( "1.0", "UTF-8" );
    my $root = $graphml->createElementNS( $graphml_ns, 'graphml' );
    $graphml->setDocumentElement( $root );
    $root->setNamespace( $xsi_ns, 'xsi', 0 );
    $root->setAttributeNS( $xsi_ns, 'schemaLocation', $graphml_schema );

    # Add the data keys for nodes
    my @node_data = ( 'name', 'token', 'identical', 'position' );
    foreach my $ndi ( 0 .. $#node_data ) {
	my $key = $root->addNewChild( $graphml_ns, 'key' );
	$key->setAttribute( 'attr.name', $node_data[$ndi] );
	$key->setAttribute( 'attr.type', 'string' );
	$key->setAttribute( 'for', 'node' );
	$key->setAttribute( 'id', 'd'.$ndi );
    }

    # Add the data keys for edges
    my %wit_hash;
    my $wit_ctr = 0;
    foreach my $wit ( $self->getWitnessList ) {
	my $wit_key = 'w' . $wit_ctr++;
	$wit_hash{$wit} = $wit_key;
	my $key = $root->addNewChild( $graphml_ns, 'key' );
	$key->setAttribute( 'attr.name', $wit );
	$key->setAttribute( 'attr.type', 'string' );
	$key->setAttribute( 'for', 'edge' );
	$key->setAttribute( 'id', $wit_key );
    }

    # Add the graph, its nodes, and its edges
    my $graph = $root->addNewChild( $graphml_ns, 'graph' );
    $graph->setAttribute( 'edgedefault', 'directed' );
    $graph->setAttribute( 'id', 'g0' ); # TODO make this meaningful
    $graph->setAttribute( 'parse.edgeids', 'canonical' );
    $graph->setAttribute( 'parse.edges', $self->edges() );
    $graph->setAttribute( 'parse.nodeids', 'canonical' );
    $graph->setAttribute( 'parse.nodes', $self->nodes() );
    $graph->setAttribute( 'parse.order', 'nodesfirst' );

    my $node_ctr = 0;
    my %node_hash;
    foreach my $n ( $self->readings ) {
	my %this_node_data = ();
	foreach my $ndi ( 0 .. $#node_data ) {
	    my $value;
	    $this_node_data{'d'.$ndi} = $n->name if $node_data[$ndi] eq 'name';
	    $this_node_data{'d'.$ndi} = $n->label 
		if $node_data[$ndi] eq 'token';
	    $this_node_data{'d'.$ndi} = $n->primary->name if $n->has_primary;
	    $this_node_data{'d'.$ndi} = 
		$self->{'positions'}->node_position( $n )
		if $node_data[$ndi] eq 'position';
	}
	my $node_el = $graph->addNewChild( $graphml_ns, 'node' );
	my $node_xmlid = 'n' . $node_ctr++;
	$node_hash{ $n->name } = $node_xmlid;
	$node_el->setAttribute( 'id', $node_xmlid );
	    
	foreach my $dk ( keys %this_node_data ) {
	    my $d_el = $node_el->addNewChild( $graphml_ns, 'data' );
	    $d_el->setAttribute( 'key', $dk );
	    $d_el->appendTextChild( $this_node_data{$dk} );
	}
    }

    foreach my $e ( $self->edges() ) {
	my( $name, $from, $to ) = ( $e->name,
				    $node_hash{ $e->from()->name() },
				    $node_hash{ $e->to()->name() } );
	my $edge_el = $graph->addNewChild( $graphml_ns, 'edge' );
	$edge_el->setAttribute( 'source', $from );
	$edge_el->setAttribute( 'target', $to );
	$edge_el->setAttribute( 'id', $name );
	# TODO Got to add the witnesses
    }

    # Return the thing
    $self->_save_graphml( $graphml );
    return $graphml;
}

=back

=item B<start>

my $beginning = $collation->start();

Returns the beginning of the collation, a meta-reading with label '#START#'.

=cut

sub start {
    # Return the beginning node of the graph.
    my $self = shift;
    my( $new_start ) = @_;
    if( $new_start ) {
	$self->del_reading( '#START#' );
	$self->graph->rename_node( $new_start, '#START#' );
    }
    return $self->reading('#START#');
}

=item B<next_word>

my $next_node = $graph->next_word( $node, $path );

Returns the node that follows the given node along the given witness
path.  TODO These are badly named.

=cut

sub next_word {
    # Return the successor via the corresponding edge.
    my $self = shift;
    return $self->_find_linked_word( 'next', @_ );
}

=item B<prior_word>

my $prior_node = $graph->prior_word( $node, $path );

Returns the node that precedes the given node along the given witness
path.  TODO These are badly named.

=cut

sub prior_word {
    # Return the predecessor via the corresponding edge.
    my $self = shift;
    return $self->_find_linked_word( 'prior', @_ );
}

sub _find_linked_word {
    my( $self, $direction, $node, $edge ) = @_;
    $edge = 'base text' unless $edge;
    my @linked_edges = $direction eq 'next' 
	? $node->outgoing() : $node->incoming();
    return undef unless scalar( @linked_edges );
    
    # We have to find the linked edge that contains all of the
    # witnesses supplied in $edge.
    my @edge_wits = split( /, /, $edge );
    foreach my $le ( @linked_edges ) {
	my @le_wits = split( /, /, $le->name() );
	if( _is_within( \@edge_wits, \@le_wits ) ) {
	    # This is the right edge.
	    return $direction eq 'next' ? $le->to() : $le->from();
	}
    }
    warn "Could not find $direction node from " . $node->label 
	. " along edge $edge";
    return undef;
}

sub create_witnesses {
    # TODO Given a new collation, make a bunch of Witness objects.

    return [];
}

no Moose;
__PACKAGE__->meta->make_immutable;
