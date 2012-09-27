package Text::Tradition::Collation::RelationshipType;

use Moose;

=head1 NAME

Text::Tradition::Collation::RelationshipType - describes a syntactic,
semantic, etc. relationship that can be made between two readings

=head1 DESCRIPTION

Text::Tradition is a library for representation and analysis of collated
texts, particularly medieval ones.  A relationship connects two readings
within a collation, usually when they appear in the same place in different
texts.

=head1 CONSTRUCTOR

=head2 new

Creates a new relationship type. Usually called via
$collation->register_relationship_type. Options include:

=over 4

=item * name - (Required string) The name of this relationship type.

=item * bindlevel - (Required int) How tightly the relationship binds. A
lower number indicates a closer binding. If A and B are related at
bindlevel 0, and B and C at bindlevel 1, it implies that A and C have the
same relationship as B and C do.

=item * is_weak - (Default false) Whether this relationship should be
replaced silently by a stronger type if requested. This is used primarily
for the internal 'collated' relationship, only to be used by parsers.

=item * is_colocation - (Default true) Whether this relationship implies
that the readings in question have parallel locations.

=item * is_transitive - (Default $self->is_colocation) Whether this
relationship type is transitive - that is, if A is related to B and C this
way, is B necessarily related to C?

=item * is_generalizable - Whether this relationship can have a non-local
scope.

=item * record_sub - A subroutine to canonify the reading text before 
determining whether individual readings match. Defaults to no canonization.

=back

=head1 ACCESSORS

=head2 name

=head2 bindlevel

=head2 is_weak

=head2 is_colocation

=head2 is_transitive

=head2 is_generalizable

=head2 record_sub

See the option descriptions above.

=cut

has 'name' => (
	is => 'ro',
	isa => 'Str',
	required => 1,
	);
	
has 'bindlevel' => (
	is => 'ro',
	isa => 'Int',
	required => 1
	);
	
has 'is_weak' => (
	is => 'ro',
	isa => 'Bool',
	default => 0,
	);
	
has 'is_colocation' => (
	is => 'ro',
	isa => 'Bool',
	default => 1
	);
	
has 'is_transitive' => (
	is => 'ro',
	isa => 'Bool',
	default => 1
	);
	
has 'is_generalizable' => (
	is => 'ro',
	isa => 'Bool',
	lazy => 1,
	default => sub { $_[0]->is_colocation }
	);
	
has 'record_sub' => (
	is => 'ro',
	isa => 'CodeRef',
	default => sub { sub { $_[0]->text } }
	);
	
# TODO Define extra validation conditions here
	
no Moose;
__PACKAGE__->meta->make_immutable;

1;
