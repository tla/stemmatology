package Traditions::Schema;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use base 'DBIx::Class::Schema';

__PACKAGE__->load_namespaces;


# Created by DBIx::Class::Schema::Loader v0.07002 @ 2010-10-19 17:34:43
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:iZNRQ5HMUi7/5s+iC1WPmg

my $database = '/home/tla/stemmatology/db/traditions.db';
__PACKAGE__->connection( "dbi:SQLite:dbname=$database" );

1;
