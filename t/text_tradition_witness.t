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

my $simple = 't/data/simple.txt';
my $s = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'Tabular',
    'file'  => $simple,
    );
my $wit_c = $s->witness( 'C' );
is( ref( $wit_c ), 'Text::Tradition::Witness' ),;
if( $wit_c ) {
    ok( !$wit_c->has_text, "Text property not yet set" );
    my $c_arr = $wit_c->text;
    is( $c_arr->[0], 'Je', "Text constructed from path" );
    ok( $wit_c->has_text, "Text property now set" );
}
}




1;
