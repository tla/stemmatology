package Text::Tradition::Collation;

use Graph::Easy;
use IPC::Run qw( run binary );
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

has 'wit_list_separator' => (
			     is => 'rw',
			     isa => 'Str',
			     default => ', ',
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
    $self->graph->use_class('node', 'Text::Tradition::Collation::Reading');

    # Pass through any graph-specific options.
    my $shape = exists( $args->{'shape'} ) ? $args->{'shape'} : 'ellipse';
    $self->graph->set_attribute( 'node', 'shape', $shape );
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
this, because Graph::Easy doesn\'t cope well with long graphs. Unless
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
    # Return the beginning reading of the graph.
    my $self = shift;
    my( $new_start ) = @_;
    if( $new_start ) {
	$self->del_reading( '#START#' );
	$self->graph->rename_node( $new_start, '#START#' );
    }
    return $self->reading('#START#');
}

=item B<next_reading>

my $next_reading = $graph->next_reading( $reading, $witpath );

Returns the reading that follows the given reading along the given witness
path.  TODO These are badly named.

=cut

sub next_reading {
    # Return the successor via the corresponding edge.
    my $self = shift;
    return $self->_find_linked_reading( 'next', @_ );
}

=item B<prior_reading>

my $prior_reading = $graph->prior_reading( $reading, $witpath );

Returns the reading that precedes the given reading along the given witness
path.  TODO These are badly named.

=cut

sub prior_reading {
    # Return the predecessor via the corresponding edge.
    my $self = shift;
    return $self->_find_linked_reading( 'prior', @_ );
}

sub _find_linked_reading {
    my( $self, $direction, $node, $edge ) = @_;
    $edge = 'base text' unless $edge;
    my @linked_edges = $direction eq 'next' 
	? $node->outgoing() : $node->incoming();
    return undef unless scalar( @linked_edges );
    
    # We have to find the linked edge that contains all of the
    # witnesses supplied in $edge.
    my @edge_wits = $self->witnesses_of_label( $edge );
    foreach my $le ( @linked_edges ) {
	my @le_wits = $self->witnesses_of_label( $le->name );
	if( _is_within( \@edge_wits, \@le_wits ) ) {
	    # This is the right edge.
	    return $direction eq 'next' ? $le->to() : $le->from();
	}
    }
    warn "Could not find $direction node from " . $node->label 
	. " along edge $edge";
    return undef;
}

# Some set logic.
sub _is_within {
    my( $set1, $set2 ) = @_;
    my $ret = 1;
    foreach my $el ( @$set1 ) {
	$ret = 0 unless grep { /^\Q$el\E$/ } @$set2;
    }
    return $ret;
}

# Walk the paths for each witness in the graph, and return the nodes
# that the graph has in common.

sub walk_witness_paths {
    my( $self, $end ) = @_;
    # For each witness, walk the path through the graph.
    # Then we need to find the common nodes.  
    # TODO This method is going to fall down if we have a very gappy 
    # text in the collation.
    my $paths = {};
    my @common_nodes;
    foreach my $wit ( @{$self->tradition->witnesses} ) {
	my $curr_reading = $self->start;
	my @wit_path = ( $curr_reading );
	my %seen_readings;
	# TODO Detect loops at some point
	while( $curr_reading->name ne $end->name ) {
	    if( $seen_readings{$curr_reading->name} ) {
		warn "Detected loop walking path for witness " . $wit->sigil
		    . " at reading " . $curr_reading->name;
		last;
	    }
	    my $next_reading = $self->next_reading( $curr_reading, 
						    $wit->sigil );
	    push( @wit_path, $next_reading );
	    $seen_readings{$curr_reading->name} = 1;
	    $curr_reading = $next_reading;
	}
	$wit->path( \@wit_path );
	if( @common_nodes ) {
	    my @cn;
	    foreach my $n ( @wit_path ) {
		push( @cn, $n ) if grep { $_ eq $n } @common_nodes;
	    }
	    @common_nodes = ();
	    push( @common_nodes, @cn );
	} else {
	    push( @common_nodes, @wit_path );
	}
    }

    # Mark all the nodes as either common or not.
    foreach my $cn ( @common_nodes ) {
	print STDERR "Setting " . $cn->name . " as common node\n";
	$cn->make_common;
    }
    foreach my $n ( $self->readings() ) {
	$n->make_variant unless $n->is_common;
    }
}

sub common_readings {
    my $self = shift;
    my @common = grep { $_->is_common } $self->readings();
    return @common;
}

# Calculate the relative positions of nodes in the graph, if they
# were not given to us.
sub calculate_positions {
    my $self = shift;

    # We have to calculate the position identifiers for each word,
    # keyed on the common nodes.  This will be 'fun'.  The end result
    # is a hash per witness, whose key is the word node and whose
    # value is its position in the text.  Common nodes are always N,1
    # so have identical positions in each text.
    my @common = $self->common_readings();

    my $node_pos = {};
    foreach my $wit ( @{$self->tradition->witnesses} ) {
	# First we walk each path, making a matrix for each witness that
	# corresponds to its eventual position identifier.  Common nodes
	# always start a new row, and are thus always in the first column.

	my $wit_matrix = [];
	my $cn = 0;  # We should hit the common readings in order.
	my $row = [];
	foreach my $wn ( @{$wit->path} ) {
	    if( $wn eq $common[$cn] ) {
		# Set up to look for the next common node, and
		# start a new row of words.
		$cn++;
		push( @$wit_matrix, $row ) if scalar( @$row );
		$row = [];
	    }
	    push( @$row, $wn );
	}
	push( @$wit_matrix, $row );  # Push the last row onto the matrix

	# Now we have a matrix per witness, so that each row in the
	# matrix begins with a common node, and continues with all the
	# variant words that appear in the witness.  We turn this into
	# real positions in row,cell format.  But we need some
	# trickery in order to make sure that each node gets assigned
	# to only one position.

	foreach my $li ( 1..scalar(@$wit_matrix) ) {
	    foreach my $di ( 1..scalar(@{$wit_matrix->[$li-1]}) ) {
		my $reading = $wit_matrix->[$li-1]->[$di-1];
		my $position = "$li,$di";
		# If we have seen this node before, we need to compare
		# its position with what went before.
		unless( $reading->has_position &&
			_cmp_position( $position, $reading->position ) < 1 ) {
		    # The new position ID replaces the old one.
		    $reading->position( $position );
		} # otherwise, the old position needs to stay.
	    }
	}
    }
}

sub _cmp_position {
    my( $a, $b ) = @_;
    my @pos_a = split(/,/, $a );
    my @pos_b = split(/,/, $b );

    my $big_cmp = $pos_a[0] <=> $pos_b[0];
    return $big_cmp if $big_cmp;
    # else 
    return $pos_a[1] <=> $pos_b[1];
}
 
# Return the string that joins together a list of witnesses for
# display on a single path.
sub path_label {
    my $self = shift;
    return join( $self->wit_list_separator, @_ );
}

sub witnesses_of_label {
    my $self = shift;
    my $regex = $self->wit_list_separator;
    return split( /^\Q$regex\E$/, @_ );
}    

no Moose;
__PACKAGE__->meta->make_immutable;
