package Text::Tradition::Collation;
use Moose;

has 'graph' => (
		is => 'ro',
		isa => 'Graph::Easy',
		writer => '_init_graph',
		handles => {
		    add_node => 'add_reading',
		    del_node => 'del_reading',
		    add_edge => 'add_path',
		    del_edge => 'del_path',
		    nodes => 'readings',
		    edges => 'paths',
		);
		

# TODO do we not have a way to access the parent object?
has 'tradition' => (
		    is => 'ro',
		    isa => 'Text::Tradition',
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

# TODO BUILDARGS

# Wrappers around some methods

sub merge_readings {
    my $self = shift;
    my $first_node = shift;
    my $second_node = shift;
    $first_node->merge_from( $second_node );
    unshift( @_, $first_node, $second_node );
    return $self->graph->merge_nodes( @_ );
}

no Moose;
__PACKAGE__->meta->make_immutable;
