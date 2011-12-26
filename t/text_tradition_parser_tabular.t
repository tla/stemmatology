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

my $csv = 't/data/florilegium.csv';
my $t = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'Tabular',
    'file'  => $csv,
    'sep_char' => ',',
    );

is( ref( $t ), 'Text::Tradition', "Parsed florilegium CSV file" );

### TODO Check these figures
if( $t ) {
    is( scalar $t->collation->readings, 312, "Collation has all readings" );
    is( scalar $t->collation->paths, 363, "Collation has all paths" );
    is( scalar $t->witnesses, 13, "Collation has all witnesses" );
}
}




1;
