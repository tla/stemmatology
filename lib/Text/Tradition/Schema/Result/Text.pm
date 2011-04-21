package Traditions::Schema::Result::Text;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

Traditions::Schema::Result::Text

=cut

__PACKAGE__->table("texts");

=head1 ACCESSORS

=head2 textid

  data_type: 'integer'
  is_auto_increment: 1
  is_nullable: 0

=head2 provenance

  data_type: 'text'
  is_nullable: 0

=head2 description

  data_type: 'text'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "textid",
  { data_type => "integer", is_auto_increment => 1, is_nullable => 0 },
  "provenance",
  { data_type => "text", is_nullable => 0 },
  "description",
  { data_type => "text", is_nullable => 1 },
);
__PACKAGE__->set_primary_key("textid");

=head1 RELATIONS

=head2 manuscripts

Type: has_many

Related object: L<Traditions::Schema::Result::Manuscript>

=cut

__PACKAGE__->has_many(
  "manuscripts",
  "Traditions::Schema::Result::Manuscript",
  { "foreign.text" => "self.textid" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 collations

Type: has_many

Related object: L<Traditions::Schema::Result::Collation>

=cut

__PACKAGE__->has_many(
  "collations",
  "Traditions::Schema::Result::Collation",
  { "foreign.text" => "self.textid" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2010-10-19 17:34:43
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:nRxu7u/rg6k397lkxT3IWQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
