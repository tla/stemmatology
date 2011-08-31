package Text::Tradition::Collation::Segment;

use Moose;
use MooseX::NonMoose;
use Text::Tradition::Collation::Position;

extends 'Graph::Easy::Node';

# A segment is a special 'invisible' node that is a set of Readings.
# We should never display these, but it is useful to have them
# available for many-to-many relationship mappings.

has 'members' => (
    is => 'rw',
    isa => 'ArrayRef[Text::Tradition::Collation::Reading]',
    required => 1,
);

has 'position' => (
    is => 'rw',
    isa => 'Text::Tradition::Collation::Position',
    predicate => 'has_position',
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
    my $ctr = 1;
    foreach my $r ( @{$args->{members}} ) {
        my $seg_edge = $r->parent->add_edge( $r, $self, $ctr++ );
        $seg_edge->set_attribute( 'class', 'segment' );
    }
    unless ( grep { !$_->has_position } @{$args->{members}} ) {
        $self->set_position;
    }
}

# We use our 'members' array for the initialization, but afterward we
# go by graph edges.  This ensures that merged nodes stay merged.
around 'members' => sub {
    my $orig = shift;
    my $self = shift;
    my @members;
    foreach my $sl ( sort { $a->name <=> $b->name } 
                     grep { $_->sub_class eq 'segment' } $self->incoming ) {
        push( @members, $sl->from );
    }
    return \@members;
};

sub set_position {
    my $self = shift;
    my( $common, $min, $max );
    my $readings = $self->members;
    foreach my $r ( @{$self->members} ) {
        if( $r->has_position ) {
            if( $common && $common != $r->position->common ) {
                warn "Segment adding node with position skew";
            } elsif( !$common ) {
                $common = $r->position->common;
            }
            $min = $r->position->min unless $min && $min < $r->position->min;
            $max = $r->position->max unless $max && $max > $r->position->max;
        } else {
            warn "Called set_position on segment which has an unpositioned reading";
        }
    }
    $self->position( Text::Tradition::Collation::Position->new( 
        common => $common, min => $min, max => $max
        ) );
}
sub neighbor_readings {
    my( $self, $direction ) = @_;
    $direction = 'both' unless $direction;
    my @answer;
    if( $direction !~ /^back/ ) {
        # We want forward readings.
        push( @answer, $self->members->[0]->neighbor_readings( 'forward' ) );
    }
    if( $direction ne 'forward' ) {
        # We want backward readings.
        push( @answer, $self->members->[0]->neighbor_readings( 'back' ) );
    }
    return @answer;
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
