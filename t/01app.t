#!/usr/bin/env perl
use strict;
use warnings;
use lib 'lib';
use Test::More;
use Text::Tradition;

BEGIN { use_ok 'Text::Tradition' }

# A simple test, just to make sure we can parse a graph.
my $datafile = 't/data/florilegium_graphml.xml';
my $tradition = Text::Tradition->new( 'input' => 'TEI',
                                      'name' => 'test0',
                                      'file' => $datafile,
                                      'linear' => 1 );

ok( $tradition, "Got a tradition object" );
is( scalar $tradition->witnesses, 13, "Found all witnesses" );
ok( $tradition->collation, "Tradition has a collation" );

my $c = $tradition->collation;
is( scalar $c->readings, 236, "Collation has all readings" );
is( scalar $c->paths, 1838, "Collation has all paths" );
is( scalar $c->relationships, 0, "Collation has all relationships" );

done_testing;