#!/usr/bin/perl

use strict; use warnings;
use Test::More;
use lib 'lib';
use Text::Tradition;
use XML::LibXML;
use XML::LibXML::XPathContext;

my $datafile = 't/data/Collatex-16.xml';

my $tradition = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'CollateX',
    'file'  => $datafile,
    );
my $collation = $tradition->collation;

# Test the svg creation
my $parser = XML::LibXML->new();
$parser->load_ext_dtd( 0 );
my $svg = $parser->parse_string( $collation->as_svg() );
is( $svg->documentElement->nodeName(), 'svg', 'Got an svg document' );

# Test for the correct number of nodes in the SVG
my $svg_xpc = XML::LibXML::XPathContext->new( $svg->documentElement() );
$svg_xpc->registerNs( 'svg', 'http://www.w3.org/2000/svg' );
my @svg_nodes = $svg_xpc->findnodes( '//svg:g[@class="node"]' );
is( scalar @svg_nodes, 26, "Correct number of nodes in the graph" );

# Test for the correct number of edges
my @svg_edges = $svg_xpc->findnodes( '//svg:g[@class="edge"]' );
is( scalar @svg_edges, 32, "Correct number of edges in the graph" );

# Test svg creation for a subgraph
my $part_svg = $parser->parse_string( $collation->as_svg( { from => 15 } ) ); # start, no end
is( $part_svg->documentElement->nodeName(), 'svg', "Got an svg subgraph to end" );
my $part_xpc = XML::LibXML::XPathContext->new( $part_svg->documentElement() );
$part_xpc->registerNs( 'svg', 'http://www.w3.org/2000/svg' );
@svg_nodes = $part_xpc->findnodes( '//svg:g[@class="node"]' );
is( scalar( @svg_nodes ), 9, 
	"Correct number of nodes in the subgraph" );
@svg_edges = $part_xpc->findnodes( '//svg:g[@class="edge"]' );
is( scalar( @svg_edges ), 10,
	"Correct number of edges in the subgraph" );
open( OUT, ">test.svg" );
print OUT $part_svg->toString();
close OUT;

$part_svg = $parser->parse_string( $collation->as_svg( { from => 10, to => 13 } ) ); # start, no end
is( $part_svg->documentElement->nodeName(), 'svg', "Got an svg subgraph in the middle" );
$part_xpc = XML::LibXML::XPathContext->new( $part_svg->documentElement() );
$part_xpc->registerNs( 'svg', 'http://www.w3.org/2000/svg' );
@svg_nodes = $part_xpc->findnodes( '//svg:g[@class="node"]' );
is( scalar( @svg_nodes ), 9, 
	"Correct number of nodes in the subgraph" );
@svg_edges = $part_xpc->findnodes( '//svg:g[@class="edge"]' );
is( scalar( @svg_edges ), 11,
	"Correct number of edges in the subgraph" );


$part_svg = $parser->parse_string( $collation->as_svg( { to => 5 } ) ); # start, no end
is( $part_svg->documentElement->nodeName(), 'svg', "Got an svg subgraph from start" );
$part_xpc = XML::LibXML::XPathContext->new( $part_svg->documentElement() );
$part_xpc->registerNs( 'svg', 'http://www.w3.org/2000/svg' );
@svg_nodes = $part_xpc->findnodes( '//svg:g[@class="node"]' );
is( scalar( @svg_nodes ), 7, 
	"Correct number of nodes in the subgraph" );
@svg_edges = $part_xpc->findnodes( '//svg:g[@class="edge"]' );
is( scalar( @svg_edges ), 7,
	"Correct number of edges in the subgraph" );

SKIP: {
	skip "lemmatization disabled for now", 1;
	# Test for the correct common nodes
	my @common_nodes = ( '#START#' );
	push( @common_nodes, qw/ n1 n5 n6 n7 n12 n16 n19 n20 n27 / );
	my @expected_nodes = map { [ $_, 1 ] } @common_nodes;
	foreach my $idx ( qw/2 3 4 8 10 11 13 16 17 18/ ) {
		splice( @expected_nodes, $idx, 0, [ "node_null", undef ] );
	}
	my @active_nodes = $collation->lemma_readings();
	subtest 'Initial common points' => \&compare_active;
	my $string = '# when ... ... ... showers sweet with ... fruit ... ... of ... has pierced ... ... ... #';
	is( make_text( @active_nodes ), $string, "Got the right starting text" );
	
	sub compare_active {
		is( scalar( @active_nodes ), scalar ( @expected_nodes ), 
		"Arrays are same length" );
	
		foreach ( 0 .. scalar(@active_nodes)-1 ) {
		is( $active_nodes[$_]->[1], $expected_nodes[$_]->[1], 
			"Element has same toggle value" );
		if( defined $active_nodes[$_]->[1] ) {
			is( $active_nodes[$_]->[0], $expected_nodes[$_]->[0], 
			"Active or toggled element has same node name " 
			. $active_nodes[$_]->[0] );
		}
		}
	}
	
	sub make_text {
		my @words;
		foreach my $n ( @_ ) {
		if( $n->[1] ) {
			push( @words, $collation->reading( $n->[0] )->text );
		} elsif ( !defined $n->[1] ) {
			push( @words, '...' );
		}
		}
		return join( ' ', @words );
	}
	
	# Test that the common nodes are marked common
	foreach my $cn ( @common_nodes ) {
		ok( $collation->reading( $cn )->is_common, "Node $cn is marked common" );
	}
	
	# Test the manuscript paths
	my $wit_a = '# when april with his showers sweet with fruit the drought of march has pierced unto the root #';
	my $wit_b = '# when showers sweet with april fruit the march of drought has pierced to the root #';
	my $wit_c = '# when showers sweet with april fruit teh drought of march has pierced teh rood #';
	is( join( ' ', @{$tradition->witness( "A" )->text} ), $wit_a, "Correct path for witness A" );
	is( join( ' ', @{$tradition->witness( "B" )->text} ), $wit_b, "Correct path for witness B" );
	is( join( ' ', @{$tradition->witness( "C" )->text} ), $wit_c, "Correct path for witness C" );
	
	# Test the transposition identifiers
	my $transposition_pools = [ [ 'n2', 'n11' ], [ 'n14', 'n18' ], 
					[ 'n17', 'n15' ] ];
	my $transposed_nodes = { 'n2' => $transposition_pools->[0],
				 'n11' => $transposition_pools->[0],
				 'n14' => $transposition_pools->[1],
				 'n15' => $transposition_pools->[2],
				 'n17' => $transposition_pools->[2],
				 'n18' => $transposition_pools->[1],
	};
	
	my $real_transposed_nodes = {};
	foreach my $r ( $collation->readings ) {
		my @same = map { $_->name } @{$r->same_as};
		$real_transposed_nodes->{ $r->name } = \@same if @same > 1;
	}
		
	is_deeply( $real_transposed_nodes, $transposed_nodes, "Found the right transpositions" );
	
	# Test turning on a node
	my @off = $collation->toggle_reading( 'n21' );
	$expected_nodes[ 16 ] = [ "n21", 1 ];
	@active_nodes = $collation->lemma_readings( @off );
	subtest 'Turned on node for new location' => \&compare_active;
	$string = '# when ... ... ... showers sweet with ... fruit ... ... of ... has pierced unto ... ... #';
	is( make_text( @active_nodes ), $string, "Got the right text" );
	 
	# Test the toggling effects of same-column
	@off = $collation->toggle_reading( 'n22' );
	splice( @expected_nodes, 16, 1, ( [ "n21", 0 ], [ "n22", 1 ] ) );
	@active_nodes = $collation->lemma_readings( @off );
	subtest 'Turned on other node in that location' => \&compare_active;
	$string = '# when ... ... ... showers sweet with ... fruit ... ... of ... has pierced to ... ... #';
	is( make_text( @active_nodes ), $string, "Got the right text" );
	
	# Test the toggling effects of transposition
	@off = $collation->toggle_reading( 'n14' );
	# Add the turned on node
	$expected_nodes[ 11 ] = [ "n14", 1 ];
	# Remove the 'off' for the previous node
	splice( @expected_nodes, 16, 1 );
	@active_nodes = $collation->lemma_readings( @off );
	subtest 'Turned on transposition node' => \&compare_active;
	$string = '# when ... ... ... showers sweet with ... fruit ... drought of ... has pierced to ... ... #';
	is( make_text( @active_nodes ), $string, "Got the right text" );
	
	@off = $collation->toggle_reading( 'n18' );
	# Toggle on the new node
	$expected_nodes[ 13 ] = [ "n18", 1 ];
	# Toggle off the transposed node
	$expected_nodes[ 11 ] = [ "n14", undef ];
	@active_nodes = $collation->lemma_readings( @off );
	subtest 'Turned on that node\'s partner' => \&compare_active;
	$string = '# when ... ... ... showers sweet with ... fruit ... ... of drought has pierced to ... ... #';
	is( make_text( @active_nodes ), $string, "Got the right text" );
	
	@off = $collation->toggle_reading( 'n14' );
	# Toggle on the new node
	$expected_nodes[ 11 ] = [ "n14", 1 ];
	# Toggle off the transposed node
	$expected_nodes[ 13 ] = [ "n18", undef ];
	@active_nodes = $collation->lemma_readings( @off );
	subtest 'Turned on the original node' => \&compare_active;
	$string = '# when ... ... ... showers sweet with ... fruit ... drought of ... has pierced to ... ... #';
	is( make_text( @active_nodes ), $string, "Got the right text" );
	
	@off = $collation->toggle_reading( 'n15' );
	# Toggle on the new node, and off with the old
	splice( @expected_nodes, 11, 1, [ "n14", 0 ], [ "n15", 1 ] );
	@active_nodes = $collation->lemma_readings( @off );
	subtest 'Turned on the colocated node' => \&compare_active;
	$string = '# when ... ... ... showers sweet with ... fruit ... march of ... has pierced to ... ... #';
	is( make_text( @active_nodes ), $string, "Got the right text" );
	
	@off = $collation->toggle_reading( 'n3' );
	# Toggle on the new node
	splice( @expected_nodes, 3, 1, [ "n3", 1 ] );
	# Remove the old toggle-off
	splice( @expected_nodes, 11, 1 );
	@active_nodes = $collation->lemma_readings( @off );
	subtest 'Turned on a singleton node' => \&compare_active;
	$string = '# when ... with ... showers sweet with ... fruit ... march of ... has pierced to ... ... #';
	is( make_text( @active_nodes ), $string, "Got the right text" );
	
	@off = $collation->toggle_reading( 'n3' );
	# Toggle off this node
	splice( @expected_nodes, 3, 1, [ "n3", 0 ] );
	@active_nodes = $collation->lemma_readings( @off );
	subtest 'Turned off a singleton node' => \&compare_active;
	$string = '# when ... ... showers sweet with ... fruit ... march of ... has pierced to ... ... #';
	is( make_text( @active_nodes ), $string, "Got the right text" );
	
	@off = $collation->toggle_reading( 'n21' );
	splice( @expected_nodes, 16, 1, ["n22", 0 ], [ "n21", 1 ] );
	@active_nodes = $collation->lemma_readings( @off );
	subtest 'Turned on another node after singleton switchoff' => \&compare_active;
	$string = '# when ... ... showers sweet with ... fruit ... march of ... has pierced unto ... ... #';
	is( make_text( @active_nodes ), $string, "Got the right text" );
	
	# Now start testing some position identifiers
	# 2. 'april with his' have no colocated
	# 3. 'april' 2 has no colocated
	# 4. 'teh' and 'the'
	# 5. 'drought' & 'march'
	# 6. 'march' & 'drought'
	# 7. 'unto' 'the' 'root'...
	#    'unto can match 'to' or 'teh'
	#    'the' can match 'teh' or 'rood'
	#    'root' can mach 'rood'
	
	foreach my $cn ( @common_nodes ) {
		my $cnr = $collation->reading( $cn );
		is( scalar( $collation->same_position_as( $cnr ) ), 0, "Node $cn has no colocations" );
	}
	
	my %expected_colocations = (
		'n2' => [],     # april
		'n3' => [],     # with
		'n4' => [],     # his
		'n11' => [],    # april
		'n8' => [ 'n13' ],  # teh -> the
		'n13' => [ 'n8' ],  # the -> teh
		'n14' => [ 'n15' ], # drought -> march
		'n18' => [ 'n17' ], # drought -> march
		'n17' => [ 'n18' ], # march -> drought
		'n15' => [ 'n14' ], # march -> drought
		'n21' => [ 'n22', 'n9' ], # unto -> to, teh
		'n22' => [ 'n21', 'n9' ], # to -> unto, teh
		'n9' => [ 'n21', 'n22', 'n23' ], # teh -> unto, to, the
		'n23' => [ 'n25', 'n9' ], # the -> teh, rood
		'n25' => [ 'n23', 'n26' ], # rood -> the, root
		'n26' => [ 'n25' ], # root -> rood
	);
	
	foreach my $n ( keys %expected_colocations ) {
		my $nr = $collation->reading( $n );
		my @colocated = sort( map { $_->name } $collation->same_position_as( $nr ) );
		is_deeply( \@colocated, $expected_colocations{$n}, "Colocated nodes for $n correct" );
	}
	
	# Test strict colocations
	$expected_colocations{'n9'} = [];
	$expected_colocations{'n21'} = ['n22'];
	$expected_colocations{'n22'} = ['n21'];
	$expected_colocations{'n23'} = [];
	$expected_colocations{'n25'} = [];
	$expected_colocations{'n26'} = [];
	
	foreach my $n ( keys %expected_colocations ) {
		my $nr = $collation->reading( $n );
		my @colocated = sort( map { $_->name } $collation->same_position_as( $nr, 1 ) );
		is_deeply( \@colocated, $expected_colocations{$n}, "Strictly colocated nodes for $n correct" );
	}
	
	# Test turning on, then off, an annoyingly overlapping node
	
	@off = $collation->toggle_reading( 'n9' );
	# Remove the old toggle-off
	splice( @expected_nodes, 16, 1 );
	splice( @expected_nodes, 17, 0, [ "n9", 1 ] );
	@active_nodes = $collation->lemma_readings( @off );
	subtest 'Turned on a node without fixed position' => \&compare_active;
	$string = '# when ... ... showers sweet with ... fruit ... march of ... has pierced unto teh ... ... #';
	is( make_text( @active_nodes ), $string, "Got the right text" );
	
	@off = $collation->toggle_reading( 'n23' );
	splice( @expected_nodes, 18, 1, [ "n23", 1 ] );
	@active_nodes = $collation->lemma_readings( @off );
	subtest 'Turned on a node colocated to one without fixed position' => \&compare_active;
	$string = '# when ... ... showers sweet with ... fruit ... march of ... has pierced unto teh the ... #';
	is( make_text( @active_nodes ), $string, "Got the right text" );
	
	@off = $collation->toggle_reading( 'n9' );
	splice( @expected_nodes, 17, 1, [ "n9", 0 ] );
	@active_nodes = $collation->lemma_readings( @off );
	subtest 'Turned on a node colocated to one without fixed position' => \&compare_active;
	$string = '# when ... ... showers sweet with ... fruit ... march of ... has pierced unto the ... #';
	is( make_text( @active_nodes ), $string, "Got the right text" );
	
	### Now test relationship madness.
	
	my( $result, @relations ) = $collation->add_relationship( 'n25', 'n23', {'type' => 'lexical'} ); # rood -> the
	ok( $result, "Added relationship between nodes" );
	is( scalar @relations, 1, "Returned only the one collapse" );
	is_deeply( $relations[0], [ 'n25', 'n23' ], "Returned the correct collapse" );
	is( $collation->reading( 'n25' )->position->reference, '9,3', "Harmonized position for n25 correct" );
	is( $collation->reading( 'n23' )->position->reference, '9,3', "Harmonized position for n23 correct" );
	is( $collation->reading( 'n9' )->position->reference, '9,2', "Adjusted position for n9 correct" );
	
	# Do some yucky hardcoded cleanup to undo this relationship.
	$collation->reading('n25')->position->max( 4 );
	$collation->reading('n9')->position->max( 3 );
	$collation->graph->del_edge( $collation->reading('n25')->edges_to( $collation->reading('n23')) );
	
	( $result, @relations ) = $collation->add_relationship( 'n26', 'n25', {'type' => 'spelling'} ); # root -> rood
	ok( $result, "Added relationship between nodes" );
	is( scalar @relations, 1, "Returned only the one collapse" );
	is_deeply( $relations[0], [ 'n26', 'n25' ], "Returned the correct collapse" );
	is( $collation->reading( 'n26' )->position->reference, '9,4', "Harmonized position for n26 correct" );
	is( $collation->reading( 'n25' )->position->reference, '9,4', "Harmonized position for n25 correct" );
	is( $collation->reading( 'n9' )->position->reference, '9,2-3', "Adjusted position for n9 correct" );
	
	( $result, @relations ) = $collation->add_relationship( 'n15', 'n9', {'type' => 'lexical'} ); # bogus march -> teh
	ok( !$result, "Refused to add skewed relationship: " . $relations[0] );
	
	( $result, @relations ) = $collation->add_relationship( 'n25', 'n26', {'type' => 'spelling'} ); # root -> rood
	ok( !$result, "Refused to add dupe relationship: " . $relations[0] );
	
	( $result, @relations ) = $collation->add_relationship( 'n8', 'n13', {'type' => 'spelling', 'global' => 1 } ); # teh -> the
	ok( $result, "Added global relationship between nodes" );
	is( scalar @relations, 2, "Returned two relationship creations" );
	is_deeply( $relations[0], [ 'n8', 'n13' ], "Returned the original collapse" );
	is_deeply( $relations[1], [ 'n9', 'n23' ], "Returned the other collapse" );
	is( $collation->reading( 'n8' )->position->reference, '6,2', "Harmonized position for n8 correct" );
	is( $collation->reading( 'n9' )->position->reference, '9,3', "Harmonized position for n9 correct" );
}

done_testing();
