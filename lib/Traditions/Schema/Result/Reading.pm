package Traditions::Schema::Result::Reading;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Core';


=head1 NAME

Traditions::Schema::Result::Reading

=cut

__PACKAGE__->table("readings");

=head1 ACCESSORS

=head2 readingid

  data_type: 'integer'
  is_auto_increment: 1
  is_nullable: 0

=head2 prior_reading

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 1

=head2 next_reading

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 1

=head2 readingtext

  data_type: 'text'
  is_nullable: 0

=head2 ante_corr

  data_type: 'text'
  is_nullable: 1

=head2 manuscript

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 0

=head2 collation

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "readingid",
  { data_type => "integer", is_auto_increment => 1, is_nullable => 0 },
  "prior_reading",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 1 },
  "next_reading",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 1 },
  "readingtext",
  { data_type => "text", is_nullable => 0 },
  "ante_corr",
  { data_type => "text", is_nullable => 1 },
  "manuscript",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 0 },
  "collation",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 1 },
);
__PACKAGE__->set_primary_key("readingid");
__PACKAGE__->add_unique_constraint("next_reading_unique", ["next_reading"]);
__PACKAGE__->add_unique_constraint("prior_reading_unique", ["prior_reading"]);

=head1 RELATIONS

=head2 manuscripts

Type: has_many

Related object: L<Traditions::Schema::Result::Manuscript>

=cut

__PACKAGE__->has_many(
  "manuscripts",
  "Traditions::Schema::Result::Manuscript",
  { "foreign.first_word" => "self.readingid" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 collation

Type: belongs_to

Related object: L<Traditions::Schema::Result::Collation>

=cut

__PACKAGE__->belongs_to(
  "collation",
  "Traditions::Schema::Result::Collation",
  { collationid => "collation" },
  {
    is_deferrable => 1,
    join_type     => "LEFT",
    on_delete     => "CASCADE",
    on_update     => "CASCADE",
  },
);

=head2 manuscript

Type: belongs_to

Related object: L<Traditions::Schema::Result::Manuscript>

=cut

__PACKAGE__->belongs_to(
  "manuscript",
  "Traditions::Schema::Result::Manuscript",
  { manuscriptid => "manuscript" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);

=head2 next_reading

Type: belongs_to

Related object: L<Traditions::Schema::Result::Reading>

=cut

__PACKAGE__->belongs_to(
  "next_reading",
  "Traditions::Schema::Result::Reading",
  { readingid => "next_reading" },
  {
    is_deferrable => 1,
    join_type     => "LEFT",
    on_delete     => "CASCADE",
    on_update     => "CASCADE",
  },
);

=head2 reading_next_reading

Type: might_have

Related object: L<Traditions::Schema::Result::Reading>

=cut

__PACKAGE__->might_have(
  "reading_next_reading",
  "Traditions::Schema::Result::Reading",
  { "foreign.next_reading" => "self.readingid" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 prior_reading

Type: belongs_to

Related object: L<Traditions::Schema::Result::Reading>

=cut

__PACKAGE__->belongs_to(
  "prior_reading",
  "Traditions::Schema::Result::Reading",
  { readingid => "prior_reading" },
  {
    is_deferrable => 1,
    join_type     => "LEFT",
    on_delete     => "CASCADE",
    on_update     => "CASCADE",
  },
);

=head2 reading_prior_reading

Type: might_have

Related object: L<Traditions::Schema::Result::Reading>

=cut

__PACKAGE__->might_have(
  "reading_prior_reading",
  "Traditions::Schema::Result::Reading",
  { "foreign.prior_reading" => "self.readingid" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2010-10-19 17:34:43
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ze+1/h74nB4r9fc6AGIIkQ


# You can replace this text with custom content, and it will be preserved on regeneration
1;
