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

my %expected_genealogical = (
	1 => '',
	2 => 1,
	3 =>  '',
	5 =>  '',
	7 =>  '',
	8 =>  '',
	10 => '',
	13 => 1,
	33 => '',
	34 => '',
	37 => '',
	60 => '',
	81 => 1,
	84 => '',
	87 => '',
	101 => '',
	102 => '',
	122 => 1,
	157 => '',
	166 => 1,
	169 => 1,
	200 => 1,
	216 => 1,
	217 => 1,
	219 => 1,
	241 => 1,
	242 => 1,
	243 => 1,
);

my $data = run_analysis( $tradition );
foreach my $row ( @{$data->{'variants'}} ) {
	is( $row->{'genealogical'}, $expected_genealogical{$row->{'id'}}, 
		"Got correct genealogical flag for row " . $row->{'id'} );
}
is( $data->{'conflict_count'}, 16, "Got right conflict count" );
is( $data->{'variant_count'}, 28, "Got right total variant number" );
}




1;
