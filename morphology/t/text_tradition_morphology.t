#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
# Test that normal form follows lemma setting. Draws on code both here and in
# the base module.

use Text::Tradition;

my $t = Text::Tradition->new(
	input => 'Self',
	file => 't/data/florilegium_graphml.xml' );
my $c = $t->collation;

# First try lemmatizing and then adding a relationship
my $r1 = $c->reading('w42');
my $r2 = $c->reading('w44');
$r1->normal_form('FOO');
$r2->normal_form('BAR');

$r1->make_lemma( 1 );
is( $r1->normal_form, 'FOO', "nothing changed yet" );
is( $r2->normal_form, 'BAR', "nothing changed yet" );

$c->add_relationship( $r1, $r2, { type => 'spelling' } );
is( $r2->normal_form, 'FOO', "Normal form followed lemma" );

# Now try setting relationships and then lemmatizing
my $r3 = $c->reading('w98');
my $r4 = $c->reading('w100');
my $r5 = $c->reading('w103');
$r3->normal_form('YAN');
$r4->normal_form('TAN');
$r5->normal_form('TETHERA');

$c->add_relationship( $r3, $r4, { type => 'orthographic', propagate => 1 } );
$c->add_relationship( $r3, $r5, { type => 'orthographic', propagate => 1 } );
is( $r3->normal_form, 'YAN', "nothing changed yet" );
is( $r4->normal_form, 'TAN', "nothing changed yet" );
is( $r5->normal_form, 'TETHERA', "nothing changed yet" );

$r3->make_lemma( 1 );
is( $r4->normal_form, 'YAN', "normal form propagated" );
is( $r5->normal_form, 'YAN', "normal form propagated" );

# Now try modifying the normal form and making sure the change is propagated
$r3->normal_form( 'JIGGIT' );
is( $r4->normal_form, 'JIGGIT', "new normal form propagated" );
is( $r5->normal_form, 'JIGGIT', "new normal form propagated" );

# ...and that no change is propagated if the reading isn't a lemma.
$r4->normal_form( 'JOLLY' );
is( $r3->normal_form, 'JIGGIT', "normal form on non-lemma not propagated" );
is( $r5->normal_form, 'JIGGIT', "normal form on non-lemma not propagated" );


# Finally, try a relationship that shouldn't propagate the normal form
my $r6 = $c->reading('w91');
my $r7 = $c->reading('w92');
$r6->normal_form('BAZ');
$r7->normal_form('QUUX');
$r6->make_lemma( 1 );

$c->add_relationship( $r6, $r7, { type => 'grammatical' } );
is( $r7->normal_form, 'QUUX', "normal form on grammatical relationship unchanged" );
}




1;
