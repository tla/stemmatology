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
my @changed = $r1->normal_form('FOO');
is_deeply( \@changed, [ $r1 ], "Normal form change produced no side effect" );
$r2->normal_form('BAR');

@changed = $r1->make_lemma( 1 );
is_deeply( \@changed, [ $r1 ], "Lemma flag changed only the concerned reading" );
is( $r1->normal_form, 'FOO', "nothing changed yet" );
is( $r2->normal_form, 'BAR', "nothing changed yet" );

@changed = ();
$c->add_relationship( $r1, $r2, { type => 'spelling' }, \@changed );
is_deeply( \@changed, [ $r2 ], "We were informed that reading 2 changed" );
is( $r2->normal_form, 'FOO', "Normal form followed lemma" );

# Now try setting relationships and then lemmatizing
my $r3 = $c->reading('w98');
my $r4 = $c->reading('w100');
my $r5 = $c->reading('w103');
$r3->normal_form('YAN');
$r4->normal_form('TAN');
$r5->normal_form('TETHERA');

@changed = ();
$c->add_relationship( $r3, $r4, { type => 'orthographic', propagate => 1 }, \@changed );
$c->add_relationship( $r3, $r5, { type => 'orthographic', propagate => 1 }, \@changed );
is( scalar( @changed ), 0, "No reading side effects yet" );
is( $r3->normal_form, 'YAN', "nothing changed yet" );
is( $r4->normal_form, 'TAN', "nothing changed yet" );
is( $r5->normal_form, 'TETHERA', "nothing changed yet" );

@changed = $r3->make_lemma( 1 );
my %present;
map { $present{$_->id} = 1 } @changed;
ok( $present{$r3->id}, "Informed of change to reading 3" );
ok( $present{$r4->id}, "Informed of change to reading 4" );
ok( $present{$r5->id}, "Informed of change to reading 5" );
is( scalar keys %present, 3, "Not informed of further changes" );
is( $r4->normal_form, 'YAN', "normal form propagated" );
is( $r5->normal_form, 'YAN', "normal form propagated" );

# Now try modifying the normal form and making sure the change is propagated
@changed = $r3->normal_form( 'JIGGIT' );
%present = ();
map { $present{$_->id} = 1 } @changed;
ok( $present{$r3->id}, "Informed of change to reading 3" );
ok( $present{$r4->id}, "Informed of change to reading 4" );
ok( $present{$r5->id}, "Informed of change to reading 5" );
is( scalar keys %present, 3, "Not informed of further changes" );
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

@changed = ();
$c->add_relationship( $r6, $r7, { type => 'grammatical' }, \@changed );
is( $r7->normal_form, 'QUUX', "normal form on grammatical relationship unchanged" );
is( scalar @changed, 0, "No readings were marked as changed" );

# Check that 'other' and 'uncertain' relationships don't propagate
my $r8 = $c->reading('w121');
my $r9 = $c->reading('w122');
$r8->normal_form('THIS');
$r9->normal_form('THAT');
$c->add_relationship( $r8, $r9, { type => 'other' } ) ;
$r8->make_lemma( 1 );
is( $r9->normal_form, 'THAT', "Normal form unchanged on non-generalizable relationship" );
}




1;
