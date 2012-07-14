#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use File::Temp;
use Test::Warn;
use Text::Tradition;
binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";
eval { no warnings; binmode $DB::OUT, ":utf8"; };

my $tradition = 't/data/florilegium_graphml.xml';
my $t = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'Self',
    'file'  => $tradition,
    );

is( ref( $t ), 'Text::Tradition', "Parsed GraphML version 2" );
if( $t ) {
    is( scalar $t->collation->readings, 319, "Collation has all readings" );
    is( scalar $t->collation->paths, 376, "Collation has all paths" );
    is( scalar $t->witnesses, 13, "Collation has all witnesses" );
}

# TODO add a relationship, add a stemma, write graphml, reparse it, check that 
# the new data is there
$t->language('Greek');
$t->add_stemma( 'dotfile' => 't/data/florilegium.dot' );
$t->collation->add_relationship( 'w12', 'w13', 
	{ 'type' => 'grammatical', 'scope' => 'global', 
	  'annotation' => 'This is some note' } );
ok( $t->collation->get_relationship( 'w12', 'w13' ), "Relationship set" );
my $graphml_str = $t->collation->as_graphml;

my $newt = Text::Tradition->new( 'input' => 'Self', 'string' => $graphml_str );
is( ref( $newt ), 'Text::Tradition', "Parsed current GraphML version" );
if( $newt ) {
    is( scalar $newt->collation->readings, 319, "Collation has all readings" );
    is( scalar $newt->collation->paths, 376, "Collation has all paths" );
    is( scalar $newt->witnesses, 13, "Collation has all witnesses" );
    is( scalar $newt->collation->relationships, 1, "Collation has added relationship" );
    is( $newt->language, 'Greek', "Tradition has correct language setting" );
    my $rel = $newt->collation->get_relationship( 'w12', 'w13' );
    ok( $rel, "Found set relationship" );
    is( $rel->annotation, 'This is some note', "Relationship has its properties" );
    is( scalar $newt->stemmata, 1, "Tradition has its stemma" );
    is( $newt->stemma(0)->witnesses, $t->stemma(0)->witnesses, "Stemma has correct length witness list" );
}

# Test user save / restore
my $fh = File::Temp->new();
my $file = $fh->filename;
$fh->close;
my $dsn = "dbi:SQLite:dbname=$file";
my $userstore = Text::Tradition::UserStore->new( { dsn => $dsn,
	extra_args => { create => 1 } } );
my $scope = $userstore->new_scope();
my $testuser = $userstore->add_user( { url => 'http://example.com' } );
is( ref( $testuser ), 'Text::Tradition::User', "Created test user via userstore" );
$testuser->add_tradition( $newt );
is( $newt->user->id, $testuser->id, "Assigned tradition to test user" );
$graphml_str = $newt->collation->as_graphml;
my $usert;
warning_is {
	$usert = Text::Tradition->new( 'input' => 'Self', 'string' => $graphml_str );
} 'DROPPING user assignment without a specified userstore',
	"Got expected user drop warning on parse";
$usert = Text::Tradition->new( 'input' => 'Self', 'string' => $graphml_str,
	'userstore' => { 'dsn' => $dsn } );
is( $usert->user->id, $testuser->id, "Parsed tradition with userstore points to correct user" );
}




1;
