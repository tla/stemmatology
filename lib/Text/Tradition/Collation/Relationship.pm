package Text::Tradition::Collation::Relationship;

use Moose;
use Moose::Util::TypeConstraints;

enum 'RelationshipType' => qw( spelling orthographic grammatical meaning lexical
							   collation repetition transposition );

enum 'RelationshipScope' => qw( local tradition global );

no Moose::Util::TypeConstraints;

=over 4

=item * type - Can be one of spelling, orthographic, grammatical, meaning, lexical, collated, repetition, transposition.  All but the last two are only valid relationships between readings that occur at the same point in the text.

=item * non_correctable - (Optional) True if the reading would not have been corrected independently.

=item * non_independent - (Optional) True if the variant is unlikely to have occurred independently in unrelated witnesses.

=item * scope - (Optional) A meta-attribute.  Can be one of 'local', 'tradition', or 'global'. Denotes whether the relationship between the two readings holds always, independent of context, either within this tradition or across all traditions.

=back

=cut

has 'type' => (
	is => 'ro',
	isa => 'RelationshipType',
	required => 1,
	);

has 'reading_a' => (
	is => 'ro',
	isa => 'Str',
	required => 1,
	);

has 'reading_b' => (
	is => 'ro',
	isa => 'Str',
	required => 1,
	);

has 'scope' => (
	is => 'ro',
	isa => 'RelationshipScope', 
	default => 'local',
	);

has 'non_correctable' => (
	is => 'ro',
	isa => 'Bool',
	);

has 'non_independent' => (
	is => 'ro',
	isa => 'Bool',
	);

# A read-only meta-Boolean attribute.
sub colocated {
	my $self = shift;
	return $self->type !~ /^(repetition|transposition)$/;
}

sub nonlocal {
	my $self = shift;
	return $self->scope ne 'local';
}

no Moose;
__PACKAGE__->meta->make_immutable;

1;
