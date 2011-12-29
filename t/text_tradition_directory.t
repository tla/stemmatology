#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use File::Temp;
use Text::Tradition;
use Text::Tradition::Stemma;
use_ok 'Text::Tradition::Directory';

my $fh = File::Temp->new();
my $file = $fh->filename;
$fh->close;
my $dsn = "dbi:SQLite:dbname=$file";

my $d = Text::Tradition::Directory->new( 'dsn' => $dsn,
    'extra_args' => { 'create' => 1 } );
is( ref $d, 'Text::Tradition::Directory', "Got directory object" );

my $t = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'Tabular',
    'file'  => 't/data/simple.txt',
    );
my $uuid = $d->save_tradition( $t );
ok( $uuid, "Saved test tradition" );

my $s = Text::Tradition::Stemma->new( 
	'collation' => $t->collation,
	'dotfile' => 't/data/simple.dot' );
my $sid = $d->save_stemma( $s );
ok( $sid, "Saved test stemma" );

is( $d->tradition( $uuid ), $t, "Correct tradition returned for id" );
is( $d->stemma( $uuid ), $s, "Correct stemma returned for id" );
is( scalar $d->tradition_ids, 1, "Only one tradition in DB" );

# Connect to a new instance
my $e = Text::Tradition::Directory->new( 'dsn' => $dsn );
is( scalar $e->tradition_ids, 1, "One tradition preloaded from DB" );
my $te = $e->tradition( $uuid );
is( $te->name, $t->name, "New instance returns correct tradition" );
my $se = $e->stemma( $uuid );
is( $se->graph, $s->graph, "New instance returns correct stemma" );
is( $e->tradition( 'NOT-A-UUID' ), undef, "Undef returned for non-tradition" );
is( $e->stemma( 'NOT-A-UUID' ), undef, "Undef returned for non-stemma" );
$te->name( "Changed name" );
my $new_id = $e->save_tradition( $te );
is( $new_id, $uuid, "Updated tradition ID did not change" );

my $f = Text::Tradition::Directory->new( 'dsn' => $dsn, 'preload' => 0 );
is( scalar $f->tradition_ids, 0, "No traditions preloaded from DB" );
### TODO This doesn't work, as I cannot get an object scope in the
### 'tradition' wrapper.
# my $tf = $f->tradition( $uuid );
# is( $tf->name, $t->name, "Next instance returns correct tradition" );
# is( $tf->name, "Changed name", "Change to tradition carried through" );
}




1;
