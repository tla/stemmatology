package Text::Tradition::Collation::Segment;

use Moose;
use MooseX::NonMoose;

extends 'Graph::Easy::Node';

# A segment is a special 'invisible' node that is a set of Readings.
# We should never display these, but it is useful to have them
# available for many-to-many relationship mappings.

has 'members' => (
    is => 'rw',
    isa => 'ArrayRef[Text::Tradition::Collation::Reading]',
    required => 1,
);

sub FOREIGNBUILDARGS {
    my $class = shift;
    my %args = @_;

    # Name the segment after its member elements.
    my $nodename = join( ' ', map { $_->name } @{$args{'members'}} );
    return ( 'name', $nodename );
}

sub BUILD {
    my( $self, $args ) = @_;
    $self->set_attribute( 'class', 'segment' );

    foreach my $r ( @{$self->members} ) {
	my $seg_edge = $r->parent->add_edge( $r, $self, 'segment' );
	$seg_edge->set_attribute( 'class', 'segment' );
    }
}

# For now, a segment has no position in the graph.  Eventually it might
# have the position of its first member.
sub has_position {
    return undef;
}

no Moose;
__PACKAGE__->meta->make_immutable;

1;

######################################################
## copied from Graph::Easy::Parser docs
######################################################
# when overriding nodes, we also need ::Anon

package Text::Tradition::Collation::Segment::Anon;
use Moose;
use MooseX::NonMoose;
extends 'Text::Tradition::Collation::Segment';
extends 'Graph::Easy::Node::Anon';
no Moose;
__PACKAGE__->meta->make_immutable;

1;
# use base qw/Text::Tradition::Collation::Segment/;
# use base qw/Graph::Easy::Node::Anon/;

######################################################
# and :::Empty

package Text::Tradition::Collation::Segment::Empty;
use Moose;
use MooseX::NonMoose;
extends 'Graph::Easy::Node::Empty';
no Moose;
__PACKAGE__->meta->make_immutable;

1;
# use base qw/Text::Tradition::Collation::Segment/;

######################################################
