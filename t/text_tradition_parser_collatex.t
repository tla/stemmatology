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

my $cxfile = 't/data/Collatex-16.xml';
my $t = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'CollateX',
    'file'  => $cxfile,
    );

is( ref( $t ), 'Text::Tradition', "Parsed a CollateX input" );
if( $t ) {
    is( scalar $t->collation->readings, 26, "Collation has all readings" );
    is( scalar $t->collation->paths, 32, "Collation has all paths" );
    is( scalar $t->witnesses, 3, "Collation has all witnesses" );
    
    # Check an 'identical' node
    my $transposed = $t->collation->reading( 'n15' );
    my @related = $transposed->related_readings;
    is( scalar @related, 1, "Reading links to transposed version" );
    is( $related[0]->id, 'n18', "Correct transposition link" );
}
}




1;
