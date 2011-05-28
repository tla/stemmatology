package Text::Tradition::Collation::Path;

use Moose;
use MooseX::NonMoose;

## CAREFUL if we want to use Moose::Util::TypeConstraints.  That
## 'from' clashes with Graph::Easy::Edge 'from', so we'll need to
## unimport TypeConstraints after defining the types.  Or else we
## would have to finally split out our types into another module.

extends 'Graph::Easy::Edge';

has 'hidden_witnesses' => (
    is => 'rw',
    isa => 'ArrayRef[Str]',
    predicate => 'has_hidden_witnesses'
);

sub BUILD {
    my $self = shift;
    $self->set_attribute( 'class', 'path' );
}

no Moose;
__PACKAGE__->meta->make_immutable;

