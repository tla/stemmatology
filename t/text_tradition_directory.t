#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use Test::Warn;
use File::Temp;
use Text::Tradition;
use_ok 'Text::Tradition::Directory';

my $fh = File::Temp->new();
my $file = $fh->filename;
$fh->close;
my $dsn = "dbi:SQLite:dbname=$file";

my $d = Text::Tradition::Directory->new( 'dsn' => $dsn,
	'extra_args' => { 'create' => 1 } );
is( ref $d, 'Text::Tradition::Directory', "Got directory object" );

my $scope = $d->new_scope;
my $t = Text::Tradition->new( 
	'name'  => 'inline', 
	'input' => 'Tabular',
	'file'  => 't/data/simple.txt',
	);
my $uuid = $d->save( $t );
ok( $uuid, "Saved test tradition" );

my $s = $t->add_stemma( 't/data/simple.dot' );
ok( $d->save( $t ), "Updated tradition with stemma" );
is( $d->tradition( $uuid ), $t, "Correct tradition returned for id" );
is( $d->tradition( $uuid )->stemma, $s, "...and it has the correct stemma" );
warning_like { $d->save( $s ) } qr/not a Text::Tradition/, "Correctly failed to save stemma directly";

my $e = Text::Tradition::Directory->new( 'dsn' => $dsn );
$scope = $e->new_scope;
is( scalar $e->tradition_ids, 1, "Directory index has our tradition" );
my $te = $e->tradition( $uuid );
is( $te->name, $t->name, "Retrieved the tradition from a new directory" );
my $sid = $e->object_to_id( $te->stemma );
warning_like { $e->tradition( $sid ) } qr/not a Text::Tradition/, "Did not retrieve stemma via tradition call";
warning_like { $e->delete( $sid ) } qr/Cannot directly delete non-Tradition object/, "Stemma object not deleted from DB";
$e->delete( $uuid );
ok( !$e->exists( $uuid ), "Object is deleted from DB" );
is( scalar $e->tradition_ids, 0, "Object is deleted from index" );
}




1;
