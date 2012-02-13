#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use Text::Tradition;
use Text::Tradition::Analysis qw/ run_analysis analyze_variant_location /;

my $datafile = 't/data/florilegium_tei_ps.xml';
my $tradition = Text::Tradition->new( 'input' => 'TEI',
                                      'name' => 'test0',
                                      'file' => $datafile );
my $s = $tradition->add_stemma( 'dotfile' => 't/data/florilegium.dot' );
is( ref( $s ), 'Text::Tradition::Stemma', "Added stemma to tradition" );

my $data = run_analysis( $tradition );
# TODO should be 21!
is( $data->{'genealogical_count'}, 42, "Got right genealogical count" );
is( $data->{'conflict_count'}, 17, "Got right conflict count" );
is( $data->{'variant_count'}, 58, "Got right total variant number" );
}




1;
