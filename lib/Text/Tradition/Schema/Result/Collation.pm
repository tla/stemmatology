package Traditions::Schema::Result::Collation;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

Traditions::Schema::Result::Collation

=cut

__PACKAGE__->table("collations");

=head1 ACCESSORS

=head2 collationid

  data_type: 'integer'
  is_auto_increment: 1
  is_nullable: 0

=head2 text

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 0

=head2 tag

  data_type: 'text'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "collationid",
  { data_type => "integer", is_auto_increment => 1, is_nullable => 0 },
  "text",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 0 },
  "tag",
  { data_type => "text", is_nullable => 1 },
);
__PACKAGE__->set_primary_key("collationid");

=head1 RELATIONS

=head2 readings

Type: has_many

Related object: L<Traditions::Schema::Result::Reading>

=cut

__PACKAGE__->has_many(
  "readings",
  "Traditions::Schema::Result::Reading",
  { "foreign.collation" => "self.collationid" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 text

Type: belongs_to

Related object: L<Traditions::Schema::Result::Text>

=cut

__PACKAGE__->belongs_to(
  "text",
  "Traditions::Schema::Result::Text",
  { textid => "text" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2010-10-19 17:34:43
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:+l31v5NCx//HtluCu+1JeQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
