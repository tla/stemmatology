#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use TryCatch;
use File::Temp;
use Text::Tradition;
use_ok 'Text::Tradition::Directory';

my $fh = File::Temp->new();
my $file = $fh->filename;
$fh->close;
my $dsn = "dbi:SQLite:dbname=$file";
my $uuid;
my $t = Text::Tradition->new( 
	'name'  => 'inline', 
	'input' => 'Tabular',
	'file'  => 't/data/simple.txt',
	);

{
	my $d = Text::Tradition::Directory->new( 'dsn' => $dsn,
		'extra_args' => { 'create' => 1 } );
	is( ref $d, 'Text::Tradition::Directory', "Got directory object" );
	
	my $scope = $d->new_scope;
	$uuid = $d->save( $t );
	ok( $uuid, "Saved test tradition" );
	
	my $s = $t->add_stemma( 't/data/simple.dot' );
	ok( $d->save( $t ), "Updated tradition with stemma" );
	is( $d->tradition( $uuid ), $t, "Correct tradition returned for id" );
	is( $d->tradition( $uuid )->stemma, $s, "...and it has the correct stemma" );
	try {
		$d->save( $s );
	} catch( Text::Tradition::Error $e ) {
		is( $e->ident, 'database error', "Got exception trying to save stemma directly" );
		like( $e->message, qr/Cannot directly save non-Tradition object/, 
			"Exception has correct message" );
	}
}
my $nt = Text::Tradition->new(
	'name' => 'CX',
	'input' => 'CollateX',
	'file' => 't/data/Collatex-16.xml',
	);
is( ref( $nt ), 'Text::Tradition', "Made new tradition" );

{
	my $f = Text::Tradition::Directory->new( 'dsn' => $dsn );
	my $scope = $f->new_scope;
	is( scalar $f->tradition_ids, 1, "Directory index has our tradition" );
	my $nuuid = $f->save( $nt );
	ok( $nuuid, "Stored second tradition" );
	is( scalar $f->tradition_ids, 2, "Directory index has both traditions" );
	my $tf = $f->tradition( $uuid );
	is( $tf->name, $t->name, "Retrieved the tradition from a new directory" );
	my $sid = $f->object_to_id( $tf->stemma );
	try {
		$f->tradition( $sid );
	} catch( Text::Tradition::Error $e ) {
		is( $e->ident, 'database error', "Got exception trying to fetch stemma directly" );
		like( $e->message, qr/not a Text::Tradition/, "Exception has correct message" );
	}
	try {
		$f->delete( $sid );
	} catch( Text::Tradition::Error $e ) {
		is( $e->ident, 'database error', "Got exception trying to delete stemma directly" );
		like( $e->message, qr/Cannot directly delete non-Tradition object/, 
			"Exception has correct message" );
	}
	$f->delete( $uuid );
	ok( !$f->exists( $uuid ), "Object is deleted from DB" );
	ok( !$f->exists( $sid ), "Object stemma also deleted from DB" );
	is( scalar $f->tradition_ids, 1, "Object is deleted from index" );
}

SKIP: {
	skip 'Have yet to figure out garbage collection', 1;
	my $g = Text::Tradition::Directory->new( 'dsn' => $dsn );
	my $scope = $g->new_scope;
	is( scalar $g->tradition_ids, 1, "Now one object in new directory index" );
}
}




1;
