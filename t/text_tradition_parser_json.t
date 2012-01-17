#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use Text::Tradition;
binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";
eval { no warnings; binmode $DB::OUT, ":utf8"; };

use_ok( 'Text::Tradition::Parser::JSON' );

open( JSFILE, 't/data/cx16.json' );
binmode JSFILE, ':utf8';
my @lines = <JSFILE>;
close JSFILE;

my $t = Text::Tradition->new(
    'name' => 'json',
    'input' => 'JSON',
    'string' => join( '', @lines ),
);

is( ref( $t ), 'Text::Tradition', "Parsed a JSON alignment" );
if( $t ) {
    is( scalar $t->collation->readings, 26, "Collation has all readings" );
    is( scalar $t->collation->paths, 32, "Collation has all paths" );
    is( scalar $t->witnesses, 3, "Collation has all witnesses" );
}
}




1;
