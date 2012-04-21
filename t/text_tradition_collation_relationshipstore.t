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
use Text::Tradition;
use TryCatch;

my $t1 = Text::Tradition->new( 'input' => 'Self', 'file' => 't/data/legendfrag.xml' );
# Test 1: try to equate nodes that are prevented with an intermediate collation
ok( $t1, "Parsed test fragment file" );
my $c1 = $t1->collation;
my $trel = $c1->get_relationship( '9,2', '9,3' );
is( ref( $trel ), 'Text::Tradition::Collation::Relationship',
	"Troublesome relationship exists" );
is( $trel->type, 'collated', "Troublesome relationship is a collation" );

# Try to make the link we want
try {
	$c1->add_relationship( '8,6', '10,3', { 'type' => 'orthographic' } );
	ok( 1, "Added cross-collation relationship as expected" );
} catch {
	ok( 0, "Existing collation blocked equivalence relationship" );
}

try {
	$c1->calculate_ranks();
	ok( 1, "Successfully calculated ranks" );
} catch {
	ok( 0, "Collation now has a cycle" );
}

# Now attempt merge of an identical reading
try {
	$c1->merge_readings( '9,3', '11,5' );
	ok( 1, "Successfully merged reading 'pontifex'" );
} catch ( Text::Tradition::Error $e ) {
	ok( 0, "Merge of mergeable readings failed: $e->message" );
	
}

# Test 2: try to equate nodes that are prevented with a real intermediate
# equivalence
my $t2 = Text::Tradition->new( 'input' => 'Self', 'file' => 't/data/legendfrag.xml' );
my $c2 = $t2->collation;
$c2->add_relationship( '9,2', '9,3', { 'type' => 'lexical' } );
my $trel2 = $c2->get_relationship( '9,2', '9,3' );
is( ref( $trel2 ), 'Text::Tradition::Collation::Relationship',
	"Created blocking relationship" );
is( $trel2->type, 'lexical', "Blocking relationship is not a collation" );
# This time the link ought to fail
try {
	$c2->add_relationship( '8,6', '10,3', { 'type' => 'orthographic' } );
	ok( 0, "Added cross-equivalent bad relationship" );
} catch {
	ok( 1, "Existing equivalence blocked crossing relationship" );
}

try {
	$c2->calculate_ranks();
	ok( 1, "Successfully calculated ranks" );
} catch {
	ok( 0, "Collation now has a cycle" );
}

# Test 3: make a straightforward pair of transpositions.
my $t3 = Text::Tradition->new( 'input' => 'Self', 'file' => 't/data/lf2.xml' );
# Test 1: try to equate nodes that are prevented with an intermediate collation
my $c3 = $t3->collation;
try {
	$c3->add_relationship( '36,4', '38,3', { 'type' => 'transposition' } );
	ok( 1, "Added straightforward transposition" );
} catch {
	ok( 0, "Failed to add normal transposition" );
}
try {
	$c3->add_relationship( '36,3', '38,2', { 'type' => 'transposition' } );
	ok( 1, "Added straightforward transposition complement" );
} catch {
	ok( 0, "Failed to add normal transposition complement" );
}

# Test 4: try to make a transposition that could be a parallel.
try {
	$c3->add_relationship( '28,2', '29,2', { 'type' => 'transposition' } );
	ok( 0, "Added bad colocated transposition" );
} catch {
	ok( 1, "Prevented bad colocated transposition" );
}

# Test 5: make the parallel, and then make the transposition again.
try {
	$c3->add_relationship( '28,3', '29,3', { 'type' => 'orthographic' } );
	ok( 1, "Equated identical readings for transposition" );
} catch {
	ok( 0, "Failed to equate identical readings" );
}
try {
	$c3->add_relationship( '28,2', '29,2', { 'type' => 'transposition' } );
	ok( 1, "Added straightforward transposition complement" );
} catch {
	ok( 0, "Failed to add normal transposition complement" );
}
}




1;
