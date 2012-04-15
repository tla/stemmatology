#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use Text::Tradition;
my $trad = Text::Tradition->new( 'name' => 'test tradition' );
my $c = $trad->collation;

# Test a plaintext witness via string
my $str = 'This is a line of text';
my $ptwit = $trad->add_witness( 
    'sigil' => 'A',
    'sourcetype' => 'plaintext',
    'string' => $str
     );
is( ref( $ptwit ), 'Text::Tradition::Witness', 'Created a witness' );
if( $ptwit ) {
    is( $ptwit->sigil, 'A', "Witness has correct sigil" );
    is( $c->path_text( $ptwit->sigil ), $str, "Witness has correct text" );
}

# Test some JSON witnesses via object
open( JSIN, 't/data/witnesses/testwit.json' ) or die "Could not open JSON test input";
binmode( JSIN, ':encoding(UTF-8)' );
my @lines = <JSIN>;
close JSIN;
$trad->add_json_witnesses( join( '', @lines ) );
is( ref( $trad->witness( 'MsAJ' ) ), 'Text::Tradition::Witness', 
	"Found first JSON witness" );
is( ref( $trad->witness( 'MsBJ' ) ), 'Text::Tradition::Witness', 
	"Found second JSON witness" );

# # Test an XML witness via file
# my $xmlwit = $trad->add_witness( 'sourcetype' => 'xmldesc', 
# 	'file' => 't/data/witnesses/teiwit.xml' );
# is( ref( $xmlwit ), 'Text::Tradition::Witness', "Created witness from XML file" );
# if( $xmlwit ) {
# 	is( $xmlwit->sigil, 'V887', "XML witness has correct sigil" );
# 	ok( $xmlwit->is_layered, "Picked up correction layer" );
# 	is( @{$xmlwit->path}, 185, "Got correct text length" );
# 	is( @{$xmlwit->uncorrected_path}, 185, "Got correct a.c. text length" );
# }

## Test use_text
}



# =begin testing
{
use Text::Tradition;
my $trad = Text::Tradition->new();

my @text = qw/ Thhis is a line of text /;
my $wit = $trad->add_witness( 
    'sigil' => 'A',
    'string' => join( ' ', @text ),
    'sourcetype' => 'plaintext',
    'identifier' => 'test witness',
     );
my $jsonstruct = $wit->export_as_json;
is( $jsonstruct->{'id'}, 'A', "got the right witness sigil" );
is( $jsonstruct->{'name'}, 'test witness', "got the right identifier" );
is( scalar @{$jsonstruct->{'tokens'}}, 6, "got six text tokens" );
foreach my $idx ( 0 .. $#text ) {
	is( $jsonstruct->{'tokens'}->[$idx]->{'t'}, $text[$idx], "tokens look OK" );
}

my @ctext = qw( when april with his showers sweet with fruit the drought of march 
				has pierced unto the root );
$trad = Text::Tradition->new(
	'input' => 'CollateX',
	'file' => 't/data/Collatex-16.xml' );

$jsonstruct = $trad->witness('A')->export_as_json;
is( $jsonstruct->{'id'}, 'A', "got the right witness sigil" );
is( $jsonstruct->{'name'}, undef, "got undef for missing identifier" );
is( scalar @{$jsonstruct->{'tokens'}}, 17, "got all text tokens" );
foreach my $idx ( 0 .. $#ctext ) {
	is( $jsonstruct->{'tokens'}->[$idx]->{'t'}, $ctext[$idx], "tokens look OK" );
}

## TODO test layertext export
}




1;
