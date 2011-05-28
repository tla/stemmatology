package Text::Tradition::Collation::Relationship;

use Moose;
use Moose::Util::TypeConstraints;
## CAREFUL in our use of Moose::Util::TypeConstraints.  That 'from'
## clashes with Graph::Easy::Edge 'from', so we'll need to unimport
## TypeConstraints after defining the types.  Or else we would have to
## finally split out our types into another module.
use MooseX::NonMoose;

extends 'Graph::Easy::Edge';

enum 'RelationshipType' => qw( spelling orthographic grammatical repetition );

subtype 'RelationshipVector',
    => as 'ArrayRef',
    => where { @$_ == 2
	       && $_->[0]->isa( 'Text::Tradition::Collation::Reading' )
	       && $_->[1]->isa( 'Text::Tradition::Collation::Reading' )
	     },
    message { 'Argument should be [ SourceReading, TargetReading ]' };

subtype 'RelationshipTokenVector',
    => as 'ArrayRef',
    => where { @$_ == 2 },
    message { 'Argument should be [ \'source\', \'target\' ]' };

no Moose::Util::TypeConstraints;  ## see comment above
		   
has 'sort' => (
    is => 'rw',
    isa => 'RelationshipType',
    required => 1,
);

has 'orig_relation' => (
    is => 'rw',
    isa => 'RelationshipVector',
    required => 1,
);

has 'related_readings' => (
    is => 'rw',
    isa => 'RelationshipTokenVector',
);

has 'global' => (
    is => 'rw',
    isa => 'Bool',
    default => 0,
);

sub FOREIGNBUILDARGS {
    my $class = shift;
    my %args = @_;

    # Make the label match our 'sort' attribute.
    my @superclass_args;
    if( exists $args{'sort'} ) {
	push( @superclass_args, 'label', $args{'sort'} );
    }
    return @superclass_args;
}

sub BUILD {
    my( $self, $args ) = @_;

    $self->set_attribute( 'class', 'relationship' );

    my( $source, $target ) = @{$self->orig_relation};
    if( $source->has_position && $target->has_position
	&& $source->position ne $target->position ) {
	die "Cannot set relationship between readings in different positions";
    }
    unless( $self->related_readings ) {
	$self->related_readings( [ $self->orig_relation->[0]->label,
				   $self->orig_relation->[1]->label ] );
    }
}

no Moose;
__PACKAGE__->meta->make_immutable;
