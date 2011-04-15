package Traditions::Graph;

use strict;
use warnings;
use Graph::Easy;
use IPC::Run qw( run binary );
use Module::Load;

sub new {
    my $proto = shift;
    my $class = ref( $proto ) || $proto;
    my %opts = ( 'on_color' => 'yellow',
		 'off_color' => 'white',
		 @_ );
    my $self = {};

    # opts can be: GraphML, base+CSV, base+CTE, TEI.  We need
    # something to parse.
    my @formats = grep { /^(GraphML|CSV|CTE|TEI)$/ } keys( %opts );
    my $format = shift( @formats );
    unless( $format ) {
	warn "No data given to create a graph: need GraphML, CSV, or TEI";
	return;
    }
    if( $format =~ /^(CSV|CTE)$/ && !exists $opts{'base'} ) {
	warn "Cannot make a graph from $format without a base text";
	return;
    }

    # Make a graph object.
    my $collation_graph = Graph::Easy->new();
    $collation_graph->set_attribute( 'node', 'shape', 'ellipse' );
    # Starting point for all texts
    my $last_node = $collation_graph->add_node( '#START#' );

    $self->{'graph'} = $collation_graph;
    bless( $self, $class );

    # Now do the parsing.
    my $mod = "Traditions::Parser::$format";
    load( $mod );
    my @args = ( $opts{ $format } );
    if( $format =~ /^(CSV|CTE)$/ ) {
	push( @args, $opts{'base'} );
    }
    $mod->can('parse')->( $self, @args );

    return $self;
}


### Graph::Easy object accessor methods
sub node {
    my $self = shift;
    return $self->{'graph'}->node( @_ );
}

sub edge {
    my $self = shift;
    return $self->{'graph'}->edge( @_ );
}

sub add_node {
    my $self = shift;
    return $self->{'graph'}->add_node( @_ );
}

sub add_edge {
    my $self = shift;
    return $self->{'graph'}->add_edge( @_ );
}

sub del_node {
    my $self = shift;
    return $self->{'graph'}->del_node( @_ );
}

sub del_edge {
    my $self = shift;
    return $self->{'graph'}->del_edge( @_ );
}

sub nodes {
    my $self = shift;
    return $self->{'graph'}->nodes( @_ );
}

sub edges {
    my $self = shift;
    return $self->{'graph'}->edges( @_ );
}

sub merge_nodes {
    my $self = shift;
    return $self->{'graph'}->merge_nodes( @_ );
}

### Helper methods for navigating the tree

sub start {
    # Return the beginning node of the graph.
    my $self = shift;
    my( $new_start ) = @_;
    if( $new_start ) {
	$self->{'graph'}->rename_node( $new_start, '#START#' );
    }
    return $self->{'graph'}->node('#START#');
}

sub save_positions {
    my( $self, $positions ) = @_;
    $self->{'positions'} = $positions;
}

sub next_word {
    # Return the successor via the corresponding edge.
    my( $self, $node, $edge ) = @_;
    $edge = "base text" unless $edge;
    my @next_edges = $node->outgoing();
    return undef unless scalar( @next_edges );
    
    foreach my $e ( @next_edges ) {
	next unless $e->label() eq $edge;
	return $e->to();
    }

    warn "Could not find node connected to edge $edge";
    return undef;
}

sub prior_word {
    # Return the predecessor via the corresponding edge.
    my( $self, $node, $edge ) = @_;
    $edge = "base text" unless $edge;
    my @prior_edges = $node->incoming();
    return undef unless scalar( @prior_edges );
    
    foreach my $e ( @prior_edges ) {
	next unless $e->label() eq $edge;
	return $e->from();
    }

    warn "Could not find node connected from edge $edge";
    return undef;
}

sub node_sequence {
    my( $self, $start, $end, $label ) = @_;
    # TODO make label able to follow a single MS
    unless( ref( $start ) eq 'Graph::Easy::Node'
	&& ref( $end ) eq 'Graph::Easy::Node' ) {
	warn "Called node_sequence without two nodes!";
	return ();
    }
    $label = 'base text' unless $label;
    my @nodes = ( $start );
    my %seen;
    my $n = $start;
    while( $n ne $end ) {
	if( exists( $seen{$n->name()} ) ) {
	    warn "Detected loop at " . $n->name();
	    last;
	}
	$seen{$n->name()} = 1;

	my @edges = $n->outgoing();
	my @relevant_edges = grep { $_->label =~ /^$label$/ } @edges;
	warn "Did not find an edge $label from node " . $n->label
	    unless scalar @relevant_edges;
	warn "Found more than one edge $label from node " . $n->label
	    unless scalar @relevant_edges == 1;
	my $next = $relevant_edges[0]->to();
	push( @nodes, $next );
	$n = $next;
    }
    # Check that the last node is our end node.
    my $last = $nodes[$#nodes];
    warn "Last node found from " . $start->label() . 
	" via path $label is not the end!"
	unless $last eq $end;

    return @nodes;
}

sub string_lemma {
    my( $self, $start, $end, $label ) = @_;

    my @nodes = $self->node_sequence( $start, $end, $label );
    my @words = map { $_->label() } @nodes;
    return join( ' ', @words );
}

## Output.  We use GraphViz for the layout because it handles large
## graphs better than Graph::Easy does natively.

sub as_svg {
    my( $self, $recalc ) = @_;
    return $self->{'svg'} if( exists $self->{'svg'} && !$recalc );
    
    $self->{'graphviz'} = $self->{'graph'}->as_graphviz()
	unless( exists $self->{'graphviz'} && !$recalc );
    
    my @cmd = qw/dot -Tsvg/;
    my( $svg, $err );
    my $in = $self->{'graphviz'};
    run( \@cmd, \$in, ">", binary(), \$svg );
    $self->{'svg'} = $svg;
    return $svg;
}

1;
__END__
#### EXAMINE BELOW ####

# Returns a list of the nodes that are currently on and the nodes for
# which an ellipsis needs to stand in.  Optionally takes a list of
# nodes that have just been turned off, to include in the list.
sub active_nodes {
    my( $self, @toggled_off_nodes ) = @_;
    
    my $all_nodes = {};
    map { $all_nodes->{ $_ } = $self->_find_position( $_ ) } keys %{$self->{node_state}};
    my $positions = _invert_hash( $all_nodes );
    my $positions_off = {};
    map { $positions_off->{ $all_nodes->{$_} } = $_ } @toggled_off_nodes;
    
    # Now for each position, we have to see if a node is on, and we
    # have to see if a node has been turned off.
    my @answer;
    foreach my $pos ( @{$self->{_all_positions}} ) {
	my $nodes = $positions->{$pos};

	# See if there is an active node for this position.
	my @active_nodes = grep { $self->{node_state}->{$_} == 1 } @$nodes;
	warn "More than one active node at position $pos!"
	    unless scalar( @active_nodes ) < 2;
	my $active;
	if( scalar( @active_nodes ) ) {
	    $active = $self->node_to_svg( $active_nodes[0]  );
	}

	# Is there a formerly active node that was toggled off?
	if( exists( $positions_off->{$pos} ) ) {
	    my $off_node = $self->node_to_svg( $positions_off->{$pos} );
	    if( $active ) {
		push( @answer, [ $off_node, 0 ], [ $active, 1 ] );
	    } elsif ( scalar @$nodes == 1 ) {
		# This was the only node at its position. No ellipsis.
		push( @answer, [ $off_node, 0 ] );
	    } else {
		# More than one node at this position, none now active.
		# Restore the ellipsis.
		push( @answer, [ $off_node, undef ] );
	    }
	# No formerly active node, so we just see if there is a currently
	# active one.
	} elsif( $active ) {
	    # Push the active node, whatever it is.
	    push( @answer, [ $active, 1 ] );
	} else {
	    # There is no change here; we need an ellipsis. Use
	    # the first node in the list, arbitrarily.
	    push( @answer, [ $self->node_to_svg( $nodes->[0] ), undef ] );
	}
    }

    return @answer;
}

# Compares two nodes according to their positions in the witness 
# index hash.
sub _by_position {
    my $self = shift;
    return _cmp_position( $self->_find_position( $a ), 
			 $self->_find_position( $b ) );
}

# Takes two position strings (X,Y) and sorts them.
sub _cmp_position {
    my @pos_a = split(/,/, $a );
    my @pos_b = split(/,/, $b );

    my $big_cmp = $pos_a[0] <=> $pos_b[0];
    return $big_cmp if $big_cmp;
    # else 
    return $pos_a[1] <=> $pos_b[1];
}
 
# Finds the position of a node in the witness index hash.  Warns if
# the same node has non-identical positions across witnesses.  Quite
# possibly should not warn.
sub _find_position {
    my $self = shift;
    my $node = shift;

    my $position;
    foreach my $wit ( keys %{$self->{indices}} ) {
	if( exists $self->{indices}->{$wit}->{$node} ) {
	    if( $position && $self->{indices}->{$wit}->{$node} ne $position ) {
		warn "Position for node $node varies between witnesses";
	    }
	    $position = $self->{indices}->{$wit}->{$node};
	}
    }

    warn "No position found for node $node" unless $position;
    return $position;
}

sub _invert_hash {
    my ( $hash, $plaintext_keys ) = @_;
    my %new_hash;
    foreach my $key ( keys %$hash ) {
        my $val = $hash->{$key};
        my $valkey = $val;
        if( $plaintext_keys 
            && ref( $val ) ) {
            $valkey = $plaintext_keys->{ scalar( $val ) };
            warn( "No plaintext value given for $val" ) unless $valkey;
        }
        if( exists ( $new_hash{$valkey} ) ) {
            push( @{$new_hash{$valkey}}, $key );
        } else {
            $new_hash{$valkey} = [ $key ];
        }
    }
    return \%new_hash;
}


# Takes a node ID to toggle; returns a list of nodes that are
# turned OFF as a result.
sub toggle_node {
    my( $self, $node_id ) = @_;
    $node_id = $self->node_from_svg( $node_id );

    # Is it a common node? If so, we don't want to turn it off.
    # Later we might want to allow it off, but give a warning.
    if( grep { $_ =~ /^$node_id$/ } @{$self->{common_nodes}} ) {
	return ();
    }

    my @nodes_off;
    # If we are about to turn on a node...
    if( !$self->{node_state}->{$node_id} ) {
	# Turn on the node.
	$self->{node_state}->{$node_id} = 1;
	# Turn off any other 'on' nodes in the same position.
	push( @nodes_off, $self->colocated_nodes( $node_id ) );
	# Turn off any node that is an identical transposed one.
	push( @nodes_off, $self->identical_nodes( $node_id ) )
	    if $self->identical_nodes( $node_id );
    } else {
	push( @nodes_off, $node_id );
    }

    # Turn off the nodes that need to be turned off.
    map { $self->{node_state}->{$_} = 0 } @nodes_off;
    return @nodes_off;
}

sub node_from_svg {
    my( $self, $node_id ) = @_;
    # TODO: implement this for real.  Need a mapping between SVG titles
    # and GraphML IDs, as created in make_graphviz.
    $node_id =~ s/^node_//;
    return $node_id;
}

sub node_to_svg {
    my( $self, $node_id ) = @_;
    # TODO: implement this for real.  Need a mapping between SVG titles
    # and GraphML IDs, as created in make_graphviz.
    $node_id = "node_$node_id";
    return $node_id;
}

sub colocated_nodes {
    my( $self, $node ) = @_;
    my @cl;

    # Get the position of the stated node.
    my $position;
    foreach my $index ( values %{$self->{indices}} ) {
	if( exists( $index->{$node} ) ) {
	    if( $position && $position ne $index->{$node} ) {
		warn "Two ms positions for the same node!";
	    }
	    $position = $index->{$node};
	}
    }
	
    # Now find the other nodes in that position, if any.
    foreach my $index ( values %{$self->{indices}} ) {
	my %location = reverse( %$index );
	push( @cl, $location{$position} )
	    if( exists $location{$position} 
		&& $location{$position} ne $node );
    }
    return @cl;
}

sub identical_nodes {
    my( $self, $node ) = @_;
    return undef unless exists $self->{transpositions} &&
	exists $self->{transpositions}->{$node};
    return $self->{transpositions}->{$node};
}

sub text_for_witness {
    my( $self, $wit ) = @_;
    # Get the witness name
    my %wit_id_for = reverse %{$self->{witnesses}};
    my $wit_id = $wit_id_for{$wit};
    unless( $wit_id ) {
        warn "Could not find an ID for witness $wit";
        return;
    }
    
    my $path = $self->{indices}->{$wit_id};
    my @nodes = sort { $self->_cmp_position( $path->{$a}, $path->{$b} ) } keys( %$path );
    my @words = map { $self->text_of_node( $_ ) } @nodes;
    return join( ' ', @words );
}

sub text_of_node {
    my( $self, $node_id ) = @_;
    my $xpath = '//g:node[@id="' . $self->node_from_svg( $node_id) .
        '"]/g:data[@key="' . $self->{nodedata}->{token} . '"]/child::text()';
    return $self->{xpc}->findvalue( $xpath );
}
1;
