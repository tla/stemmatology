package Text::Tradition::Graph;

use strict;
use warnings;
use Graph::Easy;
use IPC::Run qw( run binary );
use Module::Load;
use Text::Tradition::Graph::Position;

=head1 NAME

Text::Tradition::Graph

=head1 SYNOPSIS

 use Text::Tradition::Graph;

 my $text = Text::Tradition::Graph->new( 'GraphML' => '/my/graphml/file.xml' );
 my $text = Text::Tradition::Graph->new( 'TEI' => '/my/tei/file.xml' );
 my $text = Text::Tradition::Graph->new( 'CSV' => '/my/csv/file.csv',
                                         'base' => '/my/basefile.txt' );
 my $text = Text::Tradition::Graph->new( 'CTE' => '/my/cte/file.txt',
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
CSV in a format yet to be documented, GraphML as documented by the
CollateX tool (L<http://gregor.middell.net/collatex/>), or a Classical
Text Editor apparatus.  For CSV and Classical Text Editor files, the
user must also supply a base text to which the line numbering in the
collation file refers.

20/04/2011 Currently only CSV and GraphML are really supported.

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
    my $mod = "Text::Tradition::Parser::$format";
    load( $mod );
    my @args = ( $opts{ $format } );
    if( $format =~ /^(CSV|CTE)$/ ) {
	push( @args, $opts{'base'} );
    }
    $mod->can('parse')->( $self, @args );

    return $self;
}

=item B<make_positions>

$graph->make_positions( $common_nodes, $paths )

Create an associated Graph::Positions object that records the position
of each node in the graph.  This method call is probably in the wrong
place and will move.

=cut

sub make_positions {
    my( $self, $common_nodes, $paths ) = @_;
    my $positions = Text::Tradition::Graph::Position->new( $common_nodes, $paths );
    $self->{'positions'} = $positions;
}

=back

=head2 Graph::Easy object accessor methods

See the Graph::Easy documentation for descriptions of these functions.

=over

=item B<node>

=cut

sub node {
    my $self = shift;
    return $self->{'graph'}->node( @_ );
}

=item B<edge>

=cut

sub edge {
    my $self = shift;
    return $self->{'graph'}->edge( @_ );
}

=item B<add_node>

=cut

# Not only adds the node, but also initializes internal data
# about the node.

sub add_node {
    my $self = shift;
    my $node = $self->{'graph'}->add_node( @_ );
    $self->{'identical_nodes'}->{ $node->name() } = [ $node->name() ];
    return $node;
}

=item B<add_edge>

=cut

sub add_edge {
    my $self = shift;
    return $self->{'graph'}->add_edge( @_ );
}

=item B<del_node>

=cut

sub del_node {
    my $self = shift;
    my $node = $_[0];

    # Delete this node out of any relevant transposition pool.
    if( ref $node eq 'Graph::Easy::Node' ) {
	$node = $node->name();
    }
    my @ident = $self->identical_nodes( $node );
    if( @ident ) {
	# Get the pool.
	my $pool = $self->{'identical_nodes'}->{ $ident[0] };
	foreach my $i ( 0 .. scalar(@$pool)-1 ) {
	    if( $pool->[$i] eq $node ) {
		splice( @$pool, $i, 1 );
		last;
	    }
	}
    }
    delete $self->{'identical_nodes'}->{ $node };

    # Now delete the node.
    return $self->{'graph'}->del_node( @_ );
}

=item B<del_edge>

=cut

sub del_edge {
    my $self = shift;
    return $self->{'graph'}->del_edge( @_ );
}

=item B<nodes>

=cut

sub nodes {
    my $self = shift;
    return $self->{'graph'}->nodes( @_ );
}

=item B<edges>

=cut

sub edges {
    my $self = shift;
    return $self->{'graph'}->edges( @_ );
}

=item B<merge_nodes>

=cut

sub merge_nodes {
    my $self = shift;
    return $self->{'graph'}->merge_nodes( @_ );
}

### Helper methods for navigating the tree

=back

=head2 Graph navigation methods

=over

=item B<start>

my $node = $graph->start();

Returns the beginning node of the graph.

=cut

sub start {
    # Return the beginning node of the graph.
    my $self = shift;
    my( $new_start ) = @_;
    if( $new_start ) {
	# Fix the node transposition data
	delete $self->{'identical_nodes'}->{ $new_start->name() };
	$self->{'identical_nodes'}->{'#START#'} = [ '#START#' ];
	$self->{'graph'}->rename_node( $new_start, '#START#' );
    }
    return $self->{'graph'}->node('#START#');
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

# Some set logic.
sub _is_within {
    my( $set1, $set2 ) = @_;
    my $ret = 1;
    foreach my $el ( @$set1 ) {
	$ret = 0 unless grep { /^\Q$el\E$/ } @$set2;
    }
    return $ret;
}

=item B<node_sequence>

my @nodes = $graph->node_sequence( $first, $last, $path );

Returns the ordered list of nodes, starting with $first and ending
with $last, along the given witness path.

=cut

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

=item B<string_lemma>

my $text = $graph->string_lemma( $first, $last, $path );

Returns the whitespace-separated text, starting with $first and ending
with $last, represented in the graph along the given path.

=cut

sub string_lemma {
    my( $self, $start, $end, $label ) = @_;

    my @nodes = $self->node_sequence( $start, $end, $label );
    my @words = map { $_->label() } @nodes;
    return join( ' ', @words );
}

=back

=head2 Transposition handling methods

These should really move to their own module.  For use when the graph
has split transposed nodes in order to avoid edges that travel
backward.

=over

=item B<set_identical_node>

$graph->set_identical_node( $node, $other_node )

Tell the graph that these two nodes contain the same (transposed) reading.

=cut

sub set_identical_node {
    my( $self, $node, $same_node ) = @_;
    my $pool = $self->{'identical_nodes'}->{ $node };
    my $same_pool = $self->{'identical_nodes'}->{ $same_node };
    my %poolhash;
    foreach ( @$pool ) {
	$poolhash{$_} = 1;
    }
    foreach( @$same_pool ) {
	push( @$pool, $_ ) unless $poolhash{$_};
    }

    $self->{'identical_nodes'}->{ $same_node } = $pool;
}

=item B<set_identical_node>

my @nodes = $graph->identical_nodes( $node )

Get a list of nodes that contain the same (transposed) reading as the
given node.

=cut

sub identical_nodes {
    my( $self, $node ) = @_;
    my @others = grep { $_ !~ /^$node$/ } 
        @{$self->{'identical_nodes'}->{ $node }};
    return @others;
}

=back

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

=back

=head2 Lemmatization methods

=over

=item B<init_lemmatizer>

=cut

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

=item B<toggle_node>

my @nodes_turned_off = $graph->toggle_node( $node );

Takes a node name, and either lemmatizes or de-lemmatizes it. Returns
a list of all nodes that are de-lemmatized as a result of the toggle.

=cut

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

=item B<active_nodes>

my @state = $graph->active_nodes( @nodes_turned_off );

Takes a list of nodes that have just been turned off, and returns a
set of tuples of the form ['node', 'state'] that indicates what
changes need to be made to the graph.

=over

=item * 

A state of 1 means 'turn on this node'

=item * 

A state of 0 means 'turn off this node'

=item * 

A state of undef means 'an ellipsis belongs in the text here because
no decision has been made'

=back

=cut

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

sub colocated_nodes {
    my $self = shift;
    return $self->{'positions'}->colocated_nodes( @_ );
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

=back

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews, aurum@cpan.org

=cut

1;

