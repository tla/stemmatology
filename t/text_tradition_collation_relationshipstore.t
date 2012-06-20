#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use Text::Tradition;
use TryCatch;

use_ok( 'Text::Tradition::Collation::RelationshipStore' );

# Add some relationships, and delete them

my $cxfile = 't/data/Collatex-16.xml';
my $t = Text::Tradition->new( 
	'name'  => 'inline', 
	'input' => 'CollateX',
	'file'  => $cxfile,
	);
my $c = $t->collation;

my @v1 = $c->add_relationship( 'n21', 'n22', { 'type' => 'lexical' } );
is( scalar @v1, 1, "Added a single relationship" );
is( $v1[0]->[0], 'n21', "Got correct node 1" );
is( $v1[0]->[1], 'n22', "Got correct node 2" );
my @v2 = $c->add_relationship( 'n24', 'n23', 
	{ 'type' => 'spelling', 'scope' => 'global' } );
is( scalar @v2, 2, "Added a global relationship with two instances" );
@v1 = $c->del_relationship( 'n22', 'n21' );
is( scalar @v1, 1, "Deleted first relationship" );
@v2 = $c->del_relationship( 'n12', 'n13' );
is( scalar @v2, 2, "Deleted second global relationship" );
my @v3 = $c->del_relationship( 'n1', 'n2' );
is( scalar @v3, 0, "Nothing deleted on non-existent relationship" );
}



# =begin testing
{
use Test::Warn;
use Text::Tradition;
use TryCatch;

my $t1;
warning_is {
	$t1 = Text::Tradition->new( 'input' => 'Self', 'file' => 't/data/legendfrag.xml' );
} 'DROPPING r14.2 -> r8.1: Cannot set relationship on a meta reading',
	"Got expected relationship drop warning on parse";

# Test 1.1: try to equate nodes that are prevented with an intermediate collation
ok( $t1, "Parsed test fragment file" );
my $c1 = $t1->collation;
my $trel = $c1->get_relationship( 'r9.2', 'r9.3' );
is( ref( $trel ), 'Text::Tradition::Collation::Relationship',
	"Troublesome relationship exists" );
is( $trel->type, 'collated', "Troublesome relationship is a collation" );

# Try to make the link we want
try {
	$c1->add_relationship( 'r8.6', 'r10.3', { 'type' => 'orthographic' } );
	ok( 1, "Added cross-collation relationship as expected" );
} catch( Text::Tradition::Error $e ) {
	ok( 0, "Existing collation blocked equivalence relationship: " . $e->message );
}

try {
	$c1->calculate_ranks();
	ok( 1, "Successfully calculated ranks" );
} catch ( Text::Tradition::Error $e ) {
	ok( 0, "Collation now has a cycle: " . $e->message );
}

# Test 1.2: attempt merge of an identical reading
try {
	$c1->merge_readings( 'r9.3', 'r11.5' );
	ok( 1, "Successfully merged reading 'pontifex'" );
} catch ( Text::Tradition::Error $e ) {
	ok( 0, "Merge of mergeable readings failed: $e->message" );
	
}

# Test 1.3: attempt relationship with a meta reading (should fail)
try {
	$c1->add_relationship( 'r8.1', 'r9.2', { 'type' => 'collated' } );
	ok( 0, "Allowed a meta-reading to be used in a relationship" );
} catch ( Text::Tradition::Error $e ) {
	is( $e->message, 'Cannot set relationship on a meta reading', 
		"Relationship link prevented for a meta reading" );
}

# Test 1.4: try to break a relationship near a meta reading
$c1->add_relationship( 'r7.6', 'r7.3', { type => 'orthographic' } );
try {
	$c1->del_relationship( 'r7.6', 'r7.7' );
	$c1->del_relationship( 'r7.6', 'r7.3' );
	ok( 1, "Relationship broken with a meta reading as neighbor" );
} catch {
	ok( 0, "Relationship deletion failed with a meta reading as neighbor" );
}

# Test 2.1: try to equate nodes that are prevented with a real intermediate
# equivalence
my $t2;
warning_is {
	$t2 = Text::Tradition->new( 'input' => 'Self', 'file' => 't/data/legendfrag.xml' );
} 'DROPPING r14.2 -> r8.1: Cannot set relationship on a meta reading',
	"Got expected relationship drop warning on parse";
my $c2 = $t2->collation;
$c2->add_relationship( 'r9.2', 'r9.3', { 'type' => 'lexical' } );
my $trel2 = $c2->get_relationship( 'r9.2', 'r9.3' );
is( ref( $trel2 ), 'Text::Tradition::Collation::Relationship',
	"Created blocking relationship" );
is( $trel2->type, 'lexical', "Blocking relationship is not a collation" );
# This time the link ought to fail
try {
	$c2->add_relationship( 'r8.6', 'r10.3', { 'type' => 'orthographic' } );
	ok( 0, "Added cross-equivalent bad relationship" );
} catch ( Text::Tradition::Error $e ) {
	like( $e->message, qr/witness loop/,
		"Existing equivalence blocked crossing relationship" );
}

try {
	$c2->calculate_ranks();
	ok( 1, "Successfully calculated ranks" );
} catch ( Text::Tradition::Error $e ) {
	ok( 0, "Collation now has a cycle: " . $e->message );
}

# Test 3.1: make a straightforward pair of transpositions.
my $t3 = Text::Tradition->new( 'input' => 'Self', 'file' => 't/data/lf2.xml' );
# Test 1: try to equate nodes that are prevented with an intermediate collation
my $c3 = $t3->collation;
try {
	$c3->add_relationship( 'r36.4', 'r38.3', { 'type' => 'transposition' } );
	ok( 1, "Added straightforward transposition" );
} catch ( Text::Tradition::Error $e ) {
	ok( 0, "Failed to add normal transposition: " . $e->message );
}
try {
	$c3->add_relationship( 'r36.3', 'r38.2', { 'type' => 'transposition' } );
	ok( 1, "Added straightforward transposition complement" );
} catch ( Text::Tradition::Error $e ) {
	ok( 0, "Failed to add normal transposition complement: " . $e->message );
}

# Test 3.2: try to make a transposition that could be a parallel.
try {
	$c3->add_relationship( 'r28.2', 'r29.2', { 'type' => 'transposition' } );
	ok( 0, "Added bad colocated transposition" );
} catch ( Text::Tradition::Error $e ) {
	like( $e->message, qr/Readings appear to be colocated/,
		"Prevented bad colocated transposition" );
}

# Test 3.3: make the parallel, and then make the transposition again.
try {
	$c3->add_relationship( 'r28.3', 'r29.3', { 'type' => 'orthographic' } );
	ok( 1, "Equated identical readings for transposition" );
} catch ( Text::Tradition::Error $e ) {
	ok( 0, "Failed to equate identical readings: " . $e->message );
}
try {
	$c3->add_relationship( 'r28.2', 'r29.2', { 'type' => 'transposition' } );
	ok( 1, "Added straightforward transposition complement" );
} catch ( Text::Tradition::Error $e ) {
	ok( 0, "Failed to add normal transposition complement: " . $e->message );
}
}




1;
