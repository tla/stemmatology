package Text::Tradition::Collation::Relationship;

use Moose;
use Moose::Util::TypeConstraints;
## CAREFUL in our use of Moose::Util::TypeConstraints.  That 'from'
## clashes with Graph::Easy::Edge 'from', so we'll need to unimport
## TypeConstraints after defining the types.  Or else we would have to
## finally split out our types into another module.
use MooseX::NonMoose;

extends 'Graph::Easy::Edge';

enum 'RelationshipType' => qw( spelling orthographic grammatical repetition lexical );

subtype 'RelationshipVector',
    => as 'ArrayRef',
    => where { @$_ == 2
	       && $_->[0]->isa( 'Graph::Easy::Node' )
	       && $_->[1]->isa( 'Graph::Easy::Node' )
	     },
    message { 'Argument should be [ SourceReading, TargetReading ]' };

subtype 'RelationshipTokenVector',
    => as 'ArrayRef',
    => where { @$_ == 2 },
    message { 'Argument should be [ \'source\', \'target\' ]' };

no Moose::Util::TypeConstraints;  ## see comment above
		   
has 'type' => (
    is => 'rw',
    isa => 'RelationshipType',
    required => 1,
);

has 'this_relation' => (
    is => 'rw',
    isa => 'RelationshipVector',
    required => 1,
);

has 'primary_relation' => (
    is => 'rw',
    isa => 'RelationshipTokenVector',
);

has 'global' => (
    is => 'rw',
    isa => 'Bool',
    default => 0,
);

has 'non_correctable' => (
    is => 'rw',
    isa => 'Bool',
    );

has 'non_independent' => (
    is => 'rw',
    isa => 'Bool',
    );

sub FOREIGNBUILDARGS {
    my $class = shift;
    my %args = @_;

    # Make the label match our 'type' attribute.
    my @superclass_args;
    if( exists $args{'type'} ) {
	push( @superclass_args, 'label', $args{'type'} );
    }
    return @superclass_args;
}

sub BUILD {
    my( $self, $args ) = @_;

    $self->set_attribute( 'class', 'relationship' );

    my( $source, $target ) = @{$self->this_relation};
    if( $source->has_position && $target->has_position ) {
	# Harmonize the positions.
	$source->match_position( $target );
    }
    unless( $self->primary_relation ) {
	$self->primary_relation( [ $self->this_relation->[0]->label,
				   $self->this_relation->[1]->label ] );
    }
}

no Moose;
__PACKAGE__->meta->make_immutable;
