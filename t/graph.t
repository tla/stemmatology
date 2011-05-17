#!/usr/bin/perl

use strict; use warnings;
use Test::More;
use lib 'lib';
use Text::Tradition;
use XML::LibXML;
use XML::LibXML::XPathContext;

my $datafile = 't/data/Collatex-16.xml';

open( GRAPHFILE, $datafile ) or die "Could not open $datafile";
my @lines = <GRAPHFILE>;
close GRAPHFILE;
my $tradition = Text::Tradition->new( 'GraphML' => join( '', @lines ) );
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
is( scalar @svg_nodes, 24, "Correct number of nodes in the graph" );

# Test for the correct number of edges
my @svg_edges = $svg_xpc->findnodes( '//svg:g[@class="edge"]' );
is( scalar @svg_edges, 30, "Correct number of edges in the graph" );

# Test for the correct common nodes
my @expected_nodes = map { [ $_, 1 ] } qw/ #START# n1 n5 n6 n7 n12 n13
                                            n16 n19 n20 n23 n27 /;
foreach my $idx ( qw/2 3 4 8 11 13 16 18/ ) {
    splice( @expected_nodes, $idx, 0, [ "node_null", undef ] );
}
my @active_nodes = $collation->lemma_readings();
subtest 'Initial common points' => \&compare_active;
my $string = '# when ... ... ... showers sweet with ... fruit the ... of ... has pierced ... the ... #';
is( make_text( @active_nodes ), $string, "Got the right starting text" );

sub compare_active {
    is( scalar( @active_nodes ), scalar ( @expected_nodes ), 
	"Arrays are same length" );

    foreach ( 0 .. scalar(@active_nodes)-1 ) {
	is( $active_nodes[$_]->[1], $expected_nodes[$_]->[1], 
	    "Element has same toggle value" );
	if( defined $active_nodes[$_]->[1] ) {
	    is( $active_nodes[$_]->[0], $expected_nodes[$_]->[0], 
		"Active or toggled element has same node name" );
	}
    }
}

sub make_text {
    my @words;
    foreach my $n ( @_ ) {
	if( $n->[1] ) {
	    push( @words, $collation->reading( $n->[0] )->label );
	} elsif ( !defined $n->[1] ) {
	    push( @words, '...' );
	}
    }
    return join( ' ', @words );
}

__END__

# Test the manuscript paths
my $wit_a = '# when april with his showers sweet with fruit the drought of march has pierced unto the root #';
my $wit_b = '# when showers sweet with april fruit the march of drought has pierced to the root #';
my $wit_c = '# when showers sweet with april fruit the drought of march has pierced the rood #';
is( $collation->text_for_witness( "A" ), $wit_a, "Correct path for witness A" );
is( $collation->text_for_witness( "B" ), $wit_b, "Correct path for witness B" );
is( $collation->text_for_witness( "C" ), $wit_c, "Correct path for witness C" );

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
foreach my $n ( $collation->readings() ) {
    $transposed_nodes->{ $n->name() } = [ $n->name() ]
	unless exists $transposed_nodes->{ $n->name() };
}
is_deeply( $collation->{'identical_nodes'}, $transposed_nodes, "Found the right transpositions" );

# Test turning on a node
my @off = $collation->toggle_node( 'n25' );
$expected_nodes[ 18 ] = [ "n25", 1 ];
@active_nodes = $collation->active_nodes( @off );
subtest 'Turned on node for new location' => \&compare_active;
$string = '# when ... ... ... showers sweet with ... fruit the ... of ... has pierced ... the rood #';
is( make_text( @active_nodes ), $string, "Got the right text" );
 
# Test the toggling effects of same-column
@off = $collation->toggle_node( 'n26' );
splice( @expected_nodes, 18, 1, ( [ "n25", 0 ], [ "n26", 1 ] ) );
@active_nodes = $collation->active_nodes( @off );
subtest 'Turned on other node in that location' => \&compare_active;
$string = '# when ... ... ... showers sweet with ... fruit the ... of ... has pierced ... the root #';
is( make_text( @active_nodes ), $string, "Got the right text" );

# Test the toggling effects of transposition

@off = $collation->toggle_node( 'n14' );
# Add the turned on node
$expected_nodes[ 11 ] = [ "n14", 1 ];
# Remove the 'off' for the previous node
splice( @expected_nodes, 18, 1 );
@active_nodes = $collation->active_nodes( @off );
subtest 'Turned on transposition node' => \&compare_active;
$string = '# when ... ... ... showers sweet with ... fruit the drought of ... has pierced ... the root #';
is( make_text( @active_nodes ), $string, "Got the right text" );

@off = $collation->toggle_node( 'n18' );
# Toggle on the new node
$expected_nodes[ 13 ] = [ "n18", 1 ];
# Toggle off the transposed node
$expected_nodes[ 11 ] = [ "n14", undef ];
@active_nodes = $collation->active_nodes( @off );
subtest 'Turned on that node\'s partner' => \&compare_active;
$string = '# when ... ... ... showers sweet with ... fruit the ... of drought has pierced ... the root #';
is( make_text( @active_nodes ), $string, "Got the right text" );

@off = $collation->toggle_node( 'n14' );
# Toggle on the new node
$expected_nodes[ 11 ] = [ "n14", 1 ];
# Toggle off the transposed node
$expected_nodes[ 13 ] = [ "n18", undef ];
@active_nodes = $collation->active_nodes( @off );
subtest 'Turned on the original node' => \&compare_active;
$string = '# when ... ... ... showers sweet with ... fruit the drought of ... has pierced ... the root #';
is( make_text( @active_nodes ), $string, "Got the right text" );

@off = $collation->toggle_node( 'n15' );
# Toggle on the new node, and off with the old
splice( @expected_nodes, 11, 1, [ "n14", 0 ], [ "n15", 1 ] );
@active_nodes = $collation->active_nodes( @off );
subtest 'Turned on the colocated node' => \&compare_active;
$string = '# when ... ... ... showers sweet with ... fruit the march of ... has pierced ... the root #';
is( make_text( @active_nodes ), $string, "Got the right text" );

@off = $collation->toggle_node( 'n3' );
# Toggle on the new node
splice( @expected_nodes, 3, 1, [ "n3", 1 ] );
# Remove the old toggle-off
splice( @expected_nodes, 11, 1 );
@active_nodes = $collation->active_nodes( @off );
subtest 'Turned on a singleton node' => \&compare_active;
$string = '# when ... with ... showers sweet with ... fruit the march of ... has pierced ... the root #';
is( make_text( @active_nodes ), $string, "Got the right text" );

@off = $collation->toggle_node( 'n3' );
# Toggle off this node
splice( @expected_nodes, 3, 1, [ "n3", 0 ] );
@active_nodes = $collation->active_nodes( @off );
subtest 'Turned off a singleton node' => \&compare_active;
$string = '# when ... ... showers sweet with ... fruit the march of ... has pierced ... the root #';
is( make_text( @active_nodes ), $string, "Got the right text" );

@off = $collation->toggle_node( 'n21' );
splice( @expected_nodes, 16, 1, [ "n21", 1 ] );
@active_nodes = $collation->active_nodes( @off );
subtest 'Turned on a new node after singleton switchoff' => \&compare_active;
$string = '# when ... ... showers sweet with ... fruit the march of ... has pierced unto the root #';
is( make_text( @active_nodes ), $string, "Got the right text" );

done_testing();
