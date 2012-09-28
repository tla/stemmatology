#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use Test::Warn;
use TryCatch;
use_ok( 'Text::Tradition' ); # with Language

# Test setting and recovering language
my $t = Text::Tradition->new( input => 'Self', file => 't/data/legendfrag.xml' );
warning_like { $t->language( 'Klingon' ); } qr/^Cannot load any language/,
	"Got expected warning for setting of unsupported language";
$t->language( 'English' );
is( $t->language, 'English', "Successfully set supported language" );

# Test bad attempt to lemmatize - proper lemmatization is tested separately
my $bt = Text::Tradition->new( input => 'Self', file => 't/data/besoin.xml' );
try {
	$bt->lemmatize;
	ok( 0, "Failed to throw error on lemmatizing without language" );
} catch( Text::Tradition::Error $e ) {
	is( $e->message, "Please set a language to lemmatize a tradition",
		"Got correct error thrown for lemmatization without set language" );
} catch {
	ok( 0, "Unexpected error on bad lemmatization attempt" );
}
}




1;
