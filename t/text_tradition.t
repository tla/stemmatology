#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use_ok( 'Text::Tradition', "can use module" );

my $t = Text::Tradition->new( 'name' => 'empty' );
is( ref( $t ), 'Text::Tradition', "initialized an empty Tradition object" );
is( $t->name, 'empty', "object has the right name" );
is( scalar $t->witnesses, 0, "object has no witnesses" );

my $simple = 't/data/simple.txt';
my $s = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'Tabular',
    'file'  => $simple,
    );
is( ref( $s ), 'Text::Tradition', "initialized a Tradition object" );
is( $s->name, 'inline', "object has the right name" );
is( scalar $s->witnesses, 3, "object has three witnesses" );

my $w = $s->add_witness( 'sigil' => 'D' );
is( ref( $w ), 'Text::Tradition::Witness', "new witness created" );
is( $w->sigil, 'D', "witness has correct sigil" );
is( scalar $s->witnesses, 4, "object now has four witnesses" );

my $del = $s->del_witness( 'D' );
is( $del, $w, "Deleted correct witness" );
is( scalar $s->witnesses, 3, "object has three witnesses again" );

# TODO test initialization by witness list when we have it
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
my $wit_a = $s->witness('A');
is( ref( $wit_a ), 'Text::Tradition::Witness', "Found a witness A" );
if( $wit_a ) {
    is( $wit_a->sigil, 'A', "Witness A has the right sigil" );
}
is( $s->witness('X'), undef, "There is no witness X" );
}




1;
