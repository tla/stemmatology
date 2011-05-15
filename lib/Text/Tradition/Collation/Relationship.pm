package Text::Tradition::Collation::Relationship;

use Moose;
use Moose::Util::TypeConstraints;

enum 'RelationshipType' => qw( spelling orthographic grammatical );

has 'sort' => (
    is => 'rw',
    isa => 'RelationshipType',
    required => 1,
);

has 'reading' => (
    is => 'rw',
    isa => 'Text::Tradition::Collation::Reading',
    required => 1,
);

has 'global' => (
    is => 'rw',
    isa => 'Bool',
    default => 1,
);

 no Moose;
  __PACKAGE__->meta->make_immutable;
