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




1;
