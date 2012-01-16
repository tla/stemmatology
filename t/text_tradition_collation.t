#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use Text::Tradition;

my $cxfile = 't/data/Collatex-16.xml';
my $t = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'CollateX',
    'file'  => $cxfile,
    );
my $c = $t->collation;

is( $c->common_predecessor( $c->reading('n9'), $c->reading('n23') )->id, 
    'n20', "Found correct common predecessor" );
is( $c->common_successor( $c->reading('n9'), $c->reading('n23') )->id, 
    '#END#', "Found correct common successor" );

is( $c->common_predecessor( $c->reading('n19'), $c->reading('n17') )->id, 
    'n16', "Found correct common predecessor for readings on same path" );
is( $c->common_successor( $c->reading('n21'), $c->reading('n26') )->id, 
    '#END#', "Found correct common successor for readings on same path" );
}




1;
