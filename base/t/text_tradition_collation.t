#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use Text::Tradition;

my $cxfile = 't/data/Collatex-16.xml';
my $t = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'CollateX',
    'file'  => $cxfile,
    );
my $c = $t->collation;

my $rno = scalar $c->readings;
# Split n21 ('unto') for testing purposes
my $new_r = $c->add_reading( { 'id' => 'n21p0', 'text' => 'un', 'join_next' => 1 } );
my $old_r = $c->reading( 'n21' );
$old_r->alter_text( 'to' );
$c->del_path( 'n20', 'n21', 'A' );
$c->add_path( 'n20', 'n21p0', 'A' );
$c->add_path( 'n21p0', 'n21', 'A' );
$c->add_relationship( 'n21', 'n22', { type => 'collated', scope => 'local' } );
$c->flatten_ranks();
ok( $c->reading( 'n21p0' ), "New reading exists" );
is( scalar $c->readings, $rno, "Reading add offset by flatten_ranks" );

# Combine n3 and n4 ( with his )
$c->merge_readings( 'n3', 'n4', 1 );
ok( !$c->reading('n4'), "Reading n4 is gone" );
is( $c->reading('n3')->text, 'with his', "Reading n3 has both words" );

# Collapse n9 and n10 ( rood / root )
$c->merge_readings( 'n9', 'n10' );
ok( !$c->reading('n10'), "Reading n10 is gone" );
is( $c->reading('n9')->text, 'rood', "Reading n9 has an unchanged word" );

# Combine n21 and n21p0
my $remaining = $c->reading('n21');
$remaining ||= $c->reading('n22');  # one of these should still exist
$c->merge_readings( 'n21p0', $remaining, 1 );
ok( !$c->reading('n21'), "Reading $remaining is gone" );
is( $c->reading('n21p0')->text, 'unto', "Reading n21p0 merged correctly" );
}



# =begin testing
{
use Text::Tradition;

my $st = Text::Tradition->new( 'input' => 'Self', 'file' => 't/data/collatecorr.xml' );
is( ref( $st ), 'Text::Tradition', "Got a tradition from test file" );
ok( $st->has_witness('Ba96'), "Tradition has the affected witness" );

my $sc = $st->collation;
my $numr = 17;
ok( $sc->reading('n131'), "Tradition has the affected reading" );
is( scalar( $sc->readings ), $numr, "There are $numr readings in the graph" );
is( $sc->end->rank, 14, "There are fourteen ranks in the graph" );

# Detach the erroneously collated reading
my $newr = $sc->duplicate_reading( 'n131', 'Ba96' );
ok( $newr, "New reading was created" );
ok( $sc->reading('n131_0'), "Detached the bad collation with a new reading" );
is( scalar( $sc->readings ), $numr + 1, "A reading was added to the graph" );
is( $sc->end->rank, 10, "There are now only ten ranks in the graph" );
my $csucc = $sc->common_successor( 'n131', 'n131_0' );
is( $csucc->id, 'n136', "Found correct common successor to duped reading" ); 

# Check that the bad transposition is gone
is( $sc->get_relationship( 'n130', 'n135' ), undef, "Bad transposition relationship is gone" );

# Fix the collation
ok( $sc->merge_readings( 'n124', 'n131_0' ), "Collated the readings correctly" );
my @pairs = $sc->identical_readings( start => 'n124', end => $csucc->id );
is( $sc->end->rank, 11, "The ranks shifted appropriately" );
is( scalar @pairs, 3, "Found three more identical readings" );
$sc->flatten_ranks();
is( scalar( $sc->readings ), $numr - 3, "Now we are collated correctly" );
}



# =begin testing
{
use Text::Tradition;
use TryCatch;

my $READINGS = 311;
my $PATHS = 361;

my $datafile = 't/data/florilegium_tei_ps.xml';
my $tradition = Text::Tradition->new( 'input' => 'TEI',
                                      'name' => 'test0',
                                      'file' => $datafile,
                                      'linear' => 1 );

ok( $tradition, "Got a tradition object" );
is( scalar $tradition->witnesses, 13, "Found all witnesses" );
ok( $tradition->collation, "Tradition has a collation" );

my $c = $tradition->collation;
is( scalar $c->readings, $READINGS, "Collation has all readings" );
is( scalar $c->paths, $PATHS, "Collation has all paths" );
is( scalar $c->relationships, 0, "Collation has all relationships" );

# Add a few relationships
$c->add_relationship( 'w123', 'w125', { 'type' => 'collated' } );
$c->add_relationship( 'w193', 'w196', { 'type' => 'collated' } );
$c->add_relationship( 'w257', 'w262', { 'type' => 'transposition' } );

# Now write it to GraphML and parse it again.

my $graphml = $c->as_graphml;
my $st = Text::Tradition->new( 'input' => 'Self', 'string' => $graphml );
is( scalar $st->collation->readings, $READINGS, "Reparsed collation has all readings" );
is( scalar $st->collation->paths, $PATHS, "Reparsed collation has all paths" );
is( scalar $st->collation->relationships, 3, "Reparsed collation has new relationships" );

# Now add a stemma, write to GraphML, and look at the output.
SKIP: {
	skip "Analysis module not present", 3 unless $tradition->can( 'add_stemma' );
	my $stemma = $tradition->add_stemma( 'dotfile' => 't/data/florilegium.dot' );
	is( ref( $stemma ), 'Text::Tradition::Stemma', "Parsed dotfile into stemma" );
	is( $tradition->stemmata, 1, "Tradition now has the stemma" );
	$graphml = $c->as_graphml;
	like( $graphml, qr/digraph/, "Digraph declaration exists in GraphML" );
}
}



# =begin testing
{
use Text::Tradition;

my $cxfile = 't/data/Collatex-16.xml';
my $t = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'CollateX',
    'file'  => $cxfile,
    );
my $c = $t->collation;

# Make an svg
my $table = $c->alignment_table;
ok( $c->has_cached_table, "Alignment table was cached" );
is( $c->alignment_table, $table, "Cached table returned upon second call" );
$c->calculate_ranks;
is( $c->alignment_table, $table, "Cached table retained with no rank change" );
$c->add_relationship( 'n13', 'n23', { type => 'repetition' } );
is( $c->alignment_table, $table, "Alignment table unchanged after non-colo relationship add" );
$c->add_relationship( 'n24', 'n23', { type => 'spelling' } );
isnt( $c->alignment_table, $table, "Alignment table changed after colo relationship add" );
}



# =begin testing
{
use Text::Tradition;

my $cxfile = 't/data/Collatex-16.xml';
my $t = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'CollateX',
    'file'  => $cxfile,
    );
my $c = $t->collation;

my @common = $c->calculate_common_readings();
is( scalar @common, 8, "Found correct number of common readings" );
my @marked = sort $c->common_readings();
is( scalar @common, 8, "All common readings got marked as such" );
my @expected = qw/ n1 n11 n16 n19 n20 n5 n6 n7 /;
is_deeply( \@marked, \@expected, "Found correct list of common readings" );
}



# =begin testing
{
use Text::Tradition;

my $cxfile = 't/data/Collatex-16.xml';
my $t = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'CollateX',
    'file'  => $cxfile,
    );
my $c = $t->collation;

is( $c->common_predecessor( 'n24', 'n23' )->id, 
    'n20', "Found correct common predecessor" );
is( $c->common_successor( 'n24', 'n23' )->id, 
    '__END__', "Found correct common successor" );

is( $c->common_predecessor( 'n19', 'n17' )->id, 
    'n16', "Found correct common predecessor for readings on same path" );
is( $c->common_successor( 'n21', 'n10' )->id, 
    '__END__', "Found correct common successor for readings on same path" );
}




1;
