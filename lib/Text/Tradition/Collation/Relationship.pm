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

no Moose::Util::TypeConstraints;  ## see comment above
		   
has 'type' => (
    is => 'rw',
    isa => 'RelationshipType',
    required => 1,
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
    
has 'equal_rank' => (
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

}

no Moose;
__PACKAGE__->meta->make_immutable;
