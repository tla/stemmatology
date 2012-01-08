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

my $tradition = 't/data/florilegium_graphml.xml';
my $t = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'Self',
    'file'  => $tradition,
    );

is( ref( $t ), 'Text::Tradition', "Parsed our own GraphML" );
if( $t ) {
    is( scalar $t->collation->readings, 319, "Collation has all readings" );
    is( scalar $t->collation->paths, 376, "Collation has all paths" );
    is( scalar $t->witnesses, 13, "Collation has all witnesses" );
}
}




1;
