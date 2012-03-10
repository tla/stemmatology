#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use_ok( 'Text::Tradition::Witness', "can use module" );

my @text = qw( This is a line of text );
my $wit = Text::Tradition::Witness->new( 
    'sigil' => 'A',
    'text' => \@text,
     );
is( ref( $wit ), 'Text::Tradition::Witness', 'Created a witness' );
if( $wit ) {
    is( $wit->sigil, 'A', "Witness has correct sigil" );
    is( join( ' ', @{$wit->text} ), join( ' ', @text ), "Witness has correct text" );
}
}



# =begin testing
{
use Text::Tradition;

my @text = qw( This is a line of text );
my $wit = Text::Tradition::Witness->new( 
    'sigil' => 'A',
    'text' => \@text,
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
my $trad = Text::Tradition->new(
	'input' => 'CollateX',
	'file' => 't/data/Collatex-16.xml' );

$jsonstruct = $trad->witness('A')->export_as_json;
is( $jsonstruct->{'id'}, 'A', "got the right witness sigil" );
is( $jsonstruct->{'name'}, undef, "got undef for missing identifier" );
is( scalar @{$jsonstruct->{'tokens'}}, 17, "got all text tokens" );
foreach my $idx ( 0 .. $#ctext ) {
	is( $jsonstruct->{'tokens'}->[$idx]->{'t'}, $ctext[$idx], "tokens look OK" );
}
}




1;
