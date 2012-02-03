package Text::Tradition::Collation::Relationship;

use Moose;
use Moose::Util::TypeConstraints;

enum 'RelationshipType' => qw( spelling orthographic grammatical meaning lexical
							   collated repetition transposition );

enum 'RelationshipScope' => qw( local tradition global );

no Moose::Util::TypeConstraints;

=head1 NAME

Text::Tradition::Collation::Relationship - represents a syntactic or semantic
relationship between two readings
    
=head1 DESCRIPTION

Text::Tradition is a library for representation and analysis of collated
texts, particularly medieval ones.  A relationship connects two readings
within a collation, usually when they appear in the same place in different
texts.

=head1 CONSTRUCTOR

=head2 new

Creates a new relationship. Usually called via $collation->add_relationship.
Options include:

=over 4

=item * type - Can be one of spelling, orthographic, grammatical, meaning, lexical, collated, repetition, transposition.  All but the last two are only valid relationships between readings that occur at the same point in the text.

=item * displayform - (Optional) The reading that should be displayed if the related nodes are treated as one.

=item * non_correctable - (Optional) True if the reading would not have been corrected independently.

=item * non_independent - (Optional) True if the variant is unlikely to have occurred independently in unrelated witnesses.

=item * scope - (Optional) A meta-attribute.  Can be one of 'local', 'tradition', or 'global'. Denotes whether the relationship between the two readings holds always, independent of context, either within this tradition or across all traditions.

=back

=head1 ACCESSORS

=head2 type

=head2 displayform

=head2 scope

=head2 non_correctable

=head2 non_independent

See the option descriptions above.

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

has 'displayform' => (
	is => 'ro',
	isa => 'Str',
	predicate => 'has_displayform',
	);

has 'scope' => (
	is => 'ro',
	isa => 'RelationshipScope', 
	default => 'local',
	);

has 'non_correctable' => (
	is => 'ro',
	isa => 'Bool',
	predicate => 'noncorr_set',
	);

has 'non_independent' => (
	is => 'ro',
	isa => 'Bool',
	predicate => 'nonind_set',
	);
	
# A read-only meta-Boolean attribute.

=head2 colocated

Returns true if the relationship type is one that requires that its readings
occupy the same place in the collation.

=cut

sub colocated {
	my $self = shift;
	return $self->type !~ /^(repetition|transposition)$/;
}

=head2 nonlocal

Returns true if the relationship scope is anything other than 'local'.

=cut

sub nonlocal {
	my $self = shift;
	return $self->scope ne 'local';
}

no Moose;
__PACKAGE__->meta->make_immutable;

1;
