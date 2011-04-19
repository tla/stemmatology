package Traditions::Graph;

use strict;
use warnings;
use Graph::Easy;
use IPC::Run qw( run binary );
use Module::Load;
use Traditions::Graph::Position;

=head1 NAME

(Text?)::Traditions::Graph

=head1 SYNOPSIS

use Traditions::Graph;

my $text = Traditions::Graph->new( 'GraphML' => '/my/graphml/file.xml' );
my $text = Traditions::Graph->new( 'TEI' => '/my/tei/file.xml' );
my $text = Traditions::Graph->new( 'CSV' => '/my/csv/file.csv',
                                   'base' => '/my/basefile.txt' );
my $text = Traditions::Graph->new( 'CTE' => '/my/cte/file.txt',
                                   'base' => '/my/basefile.txt' );

my $svg_string = $text->as_svg();

my $lemma_nodes = $text->active_nodes();
$text->toggle_node( 'some_word' );

=head1 DESCRIPTION

A text tradition is the representation of our knowledge of a text that
has been passed down via manuscript copies from a time before printing
presses.  Each text has a number of witnesses, that is, manuscripts
that bear a version of the text.  The tradition is the aggregation of
these witnesses, which is to say, the collation of the text.

This module takes a text collation and represents it as a horizontal
directed graph, suitable for SVG rendering and for analysis of various
forms.  Since this module was written by a medievalist, it also
provides a facility for making a critical text reconstruction by
choosing certain variants to be 'lemma' text - that is, text which
should be considered the 'standard' reading.

Although the graph is a very good way to render text collation, and is
visually very easy for a human to interpret, it doesn't have any
inherent information about which nodes 'go together' - that is, which
text readings appear in the same place as other readings.  This module
therefore calculates 'positions' on the graph, thus holding some
information about which readings can and can't be substituted for
others.

=head1 METHODS

=over 4

=item B<new>

Constructor.  Takes a source collation file from which to construct
the initial graph.  This file can be TEI (parallel segmentation) XML,
CSV in a format yet to be documented, GraphML as documented (someday)
by CollateX, or a Classical Text Editor apparatus.  For CSV and
Classical Text Editor files, the user must also supply a base text to
which the line numbering in the collation file refers.

=cut

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

sub set_identical_nodes {
    my( $self, $node_hash ) = @_;
    $self->{'identical_nodes'} = $node_hash;
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

## Methods for lemmatizing a text.

sub init_lemmatizer {
    my $self = shift;
    # Initialize the 'lemma' hash, going through all the nodes and seeing
    # which ones are common nodes.  This should only be run once.

    return if( $self->{'lemmatizer_initialized'} );
    my @active_names = map { $_->name } grep { $self->is_common( $_ ) }
        $self->nodes();
    $self->{'positions'}->init_lemmatizer( @active_names );
    $self->{'lemmatizer_initialized'} = 1;

}

sub make_positions {
    my( $self, $common_nodes, $paths ) = @_;
    my $positions = Traditions::Graph::Position->new( $common_nodes, $paths );
    $self->{'positions'} = $positions;
}

# Takes a list of nodes that have just been turned off, and returns a
# set of tuples of the form ['node', 'state'] that indicates what
# changes need to be made to the graph.
# A state of 1 means 'turn on this node'
# A state of 0 means 'turn off this node'
# A state of undef means 'an ellipsis belongs in the text here because
#   no decision has been made'
sub active_nodes {
    my( $self, @toggled_off_nodes ) = @_;

    # In case this is the first run
    $self->init_lemmatizer();
    # First get the positions of those nodes which have been
    # toggled off.
    my $positions_off = {};
    map { $positions_off->{ $self->{'positions'}->node_position( $_ ) } = $_ }
	      @toggled_off_nodes;
 
    
    # Now for each position, we have to see if a node is on, and we
    # have to see if a node has been turned off.
    my @answer;
    foreach my $pos ( $self->{'positions'}->all() ) {
	# Find the state of this position.  If there is an active node,
	# its name will be the state; otherwise the state will be 0 
	# (nothing at this position) or undef (ellipsis at this position)
	my $active = $self->{'positions'}->state( $pos );
	
	# Is there a formerly active node that was toggled off?
	if( exists( $positions_off->{$pos} ) ) {
	    my $off_node = $positions_off->{$pos};
	    if( $active && $active ne $off_node) {
		push( @answer, [ $off_node, 0 ], [ $active, 1 ] );
	    } else {
		push( @answer, [ $off_node, $active ] );
	    }

	# No formerly active node, so we just see if there is a currently
	# active one.
	} elsif( $active ) {
	    # Push the active node, whatever it is.
	    push( @answer, [ $active, 1 ] );
	} else {
	    # Push the state that is there. Arbitrarily use the first node
	    # at that position.
	    my @pos_nodes = $self->{'positions'}->nodes_at_position( $pos );
	    push( @answer, 
		  [ $pos_nodes[0], $self->{'positions'}->state( $pos ) ] );
	}
    }
    
    return @answer;
}

# A couple of helpers. TODO These should be gathered in the same place
# eventually

sub is_common {
    my( $self, $node ) = @_;
    $node = $self->_nodeobj( $node );
    return $node->get_attribute('class') eq 'common';
}

sub _nodeobj {
    my( $self, $node ) = @_;
    unless( ref $node eq 'Graph::Easy::Node' ) {
	$node = $self->node( $node );
    }
    return $node;
}

# toggle_node takes a node name, and either lemmatizes or de-lemmatizes it.
# Returns a list of nodes that are de-lemmatized as a result of the toggle.

sub toggle_node {
    my( $self, $node ) = @_;
    
    # In case this is being called for the first time.
    $self->init_lemmatizer();

    if( $self->is_common( $node ) ) {
	# Do nothing, it's a common node.
	return;
    } 
    
    my $pos = $self->{'positions'}->node_position( $node );
    my $old_state = $self->{'positions'}->state( $pos );
    my @nodes_off;
    if( $old_state && $old_state eq $node ) {
	# Turn off the node. We turn on no others by default.
	push( @nodes_off, $node );
    } else {
	# Turn on the node.
	$self->{'positions'}->set_state( $pos, $node );
	# Any other 'on' nodes in the same position should be off.
	push( @nodes_off, $self->colocated_nodes( $node ) );
	# Any node that is an identical transposed one should be off.
	push( @nodes_off, $self->identical_nodes( $node ) )
	    if $self->identical_nodes( $node );
    }
    @nodes_off = unique_list( @nodes_off );

    # Turn off the nodes that need to be turned off.
    my @nodes_turned_off;
    foreach my $n ( @nodes_off ) {
	my $npos = $self->{'positions'}->node_position( $n );
	my $state = $self->{'positions'}->state( $npos );
	if( $state && $state eq $n ) { 
	    # this node is still on
	    push( @nodes_turned_off, $n );
	    my $new_state = undef;
	    if( $n eq $node ) {
		# This is the node that was clicked, so if there are no
		# other nodes there, turn off the position.  In all other
		# cases, restore the ellipsis.
		my @all_n = $self->{'positions'}->nodes_at_position( $pos );
		$new_state = 0 if scalar( @all_n ) == 1;
	    }
	    $self->{'positions'}->set_state( $npos, $new_state );
	} elsif( $old_state && $old_state eq $n ) { 
	    # another node has already been turned on here
	    push( @nodes_turned_off, $n );
	} # else some other node was on anyway, so pass.
    }
    return @nodes_turned_off;
}

sub colocated_nodes {
    my $self = shift;
    return $self->{'positions'}->colocated_nodes( @_ );
}

sub identical_nodes {
    my( $self, $node ) = @_;
    return undef unless exists $self->{'identical_nodes'} &&
	exists $self->{'identical_nodes'}->{$node};
    return $self->{'identical_nodes'}->{$node};
}

sub text_of_node {
    my( $self, $node_id ) = @_;
    # This is the label of the given node.
    return $self->node( $node_id )->label();
}

sub text_for_witness {
    my( $self, $wit ) = @_;
    
    my @nodes = $self->{'positions'}->witness_path( $wit );
    my @words = map { $self->node( $_ )->label() } @nodes;
    return join( ' ', @words );
}

sub unique_list {
    my( @list ) = @_;
    my %h;
    map { $h{$_} = 1 } @list;
    return keys( %h );
}

1;

