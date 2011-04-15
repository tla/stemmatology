#!/usr/bin/perl

use strict; use warnings;
use Test::More;
use lib 'lib';
use lemmatizer::Model::Graph;
use XML::LibXML;
use XML::LibXML::XPathContext;

my $datafile = 't/data/Collatex-16.xml';

open( GRAPHFILE, $datafile ) or die "Could not open $datafile";
my @lines = <GRAPHFILE>;
close GRAPHFILE;
my $graph = lemmatizer::Model::Graph->new( 'xml' => join( '', @lines ) );

# Test the svg creation
my $parser = XML::LibXML->new();
$parser->load_ext_dtd( 0 );
my $svg = $parser->parse_string( $graph->as_svg() );
is( $svg->documentElement->nodeName(), 'svg', 'Got an svg document' );

# Test for the correct number of nodes in the SVG
my $svg_xpc = XML::LibXML::XPathContext->new( $svg->documentElement() );
$svg_xpc->registerNs( 'svg', 'http://www.w3.org/2000/svg' );
my @svg_nodes = $svg_xpc->findnodes( '//svg:g[@class="node"]' );
is( scalar @svg_nodes, 21, "Correct number of nodes in the graph" );

# Test for the correct number of edges
my @svg_edges = $svg_xpc->findnodes( '//svg:g[@class="edge"]' );
is( scalar @svg_edges, 27, "Correct number of edges in the graph" );

# Test for the correct common nodes
my @expected_nodes = map { [ "node_$_", 1 ] } qw/0 1 8 12 13 16 19 20 23 27/;
foreach my $idx ( qw/2 3 5 8 10 13 15/ ) {
    splice( @expected_nodes, $idx, 0, [ "node_null", undef ] );
}
my @active_nodes = $graph->active_nodes();
# is_deeply( \@active_nodes, \@expected_nodes, "Initial common points" );
subtest 'Initial common points' => \&compare_active;
my $string = '# when ... ... showers sweet with ... fruit the ... of ... has pierced ... the ... #';
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
	    push( @words, $graph->text_of_node( $n->[0] ) );
	} elsif ( !defined $n->[1] ) {
	    push( @words, '...' );
	}
    }
    return join( ' ', @words );
}

# Test the manuscript paths
my $wit_a = '# when april with his showers sweet with fruit the drought of march has pierced unto the root #';
my $wit_b = '# when showers sweet with april fruit the march of drought has pierced to the root #';
my $wit_c = '# when showers sweet with april fruit the drought of march has pierced the rood #';
is( $graph->text_for_witness( "A" ), $wit_a, "Correct path for witness A" );
is( $graph->text_for_witness( "B" ), $wit_b, "Correct path for witness B" );
is( $graph->text_for_witness( "C" ), $wit_c, "Correct path for witness C" );

# Test the transposition identifiers
my $transposed_nodes = { 2 => 9,
			 9 => 2,
			 14 => 18,
			 15 => 17,
			 17 => 15,
			 18 => 14
};
is_deeply( $graph->{transpositions}, $transposed_nodes, "Found the right transpositions" );

# Test turning on a node
my @off = $graph->toggle_node( 'node_24' );
$expected_nodes[ 15 ] = [ "node_24", 1 ];
splice( @expected_nodes, 15, 1, ( [ "node_26", 0 ], [ "node_24", 1 ] ) );
@active_nodes = $graph->active_nodes( @off );
subtest 'Turned on node for new location' => \&compare_active;
$string = '# when ... ... showers sweet with ... fruit the ... of ... has pierced ... the root #';
is( make_text( @active_nodes ), $string, "Got the right text" );
 
# Test the toggling effects of same-column
@off = $graph->toggle_node( 'node_26' );
splice( @expected_nodes, 15, 2, ( [ "node_24", 0 ], [ "node_26", 1 ] ) );
@active_nodes = $graph->active_nodes( @off );
subtest 'Turned on other node in that location' => \&compare_active;
$string = '# when ... ... showers sweet with ... fruit the ... of ... has pierced ... the rood #';
is( make_text( @active_nodes ), $string, "Got the right text" );

# Test the toggling effects of transposition

@off = $graph->toggle_node( 'node_14' );
# Add the turned on node
splice( @expected_nodes, 8, 1, ( [ "node_15", 0 ], [ "node_14", 1 ] ) );
# Add the off transposition node
splice( @expected_nodes, 11, 1, [ "node_18", undef ] );
# Remove the explicit turning off of the earlier node
splice( @expected_nodes, 16, 1 );
@active_nodes = $graph->active_nodes( @off );
subtest 'Turned on transposition node' => \&compare_active;
$string = '# when ... ... showers sweet with ... fruit the drought of ... has pierced ... the rood #';
is( make_text( @active_nodes ), $string, "Got the right text" );

@off = $graph->toggle_node( 'node_18' );
splice( @expected_nodes, 8, 2, [ "node_14", undef ] );
splice( @expected_nodes, 10, 1, ( [ "node_17", 0 ], [ "node_18", 1 ] ) );
@active_nodes = $graph->active_nodes( @off );
subtest 'Turned on that node\'s partner' => \&compare_active;
$string = '# when ... ... showers sweet with ... fruit the ... of drought has pierced ... the rood #';
is( make_text( @active_nodes ), $string, "Got the right text" );

@off = $graph->toggle_node( 'node_14' );
splice( @expected_nodes, 8, 1, [ "node_15", 0 ], [ "node_14", 1 ] );
splice( @expected_nodes, 11, 2, ( [ "node_18", undef ] ) );
@active_nodes = $graph->active_nodes( @off );
subtest 'Turned on the original node' => \&compare_active;
$string = '# when ... ... showers sweet with ... fruit the drought of ... has pierced ... the rood #';
is( make_text( @active_nodes ), $string, "Got the right text" );

@off = $graph->toggle_node( 'node_3' );
splice( @expected_nodes, 3, 1, [ "node_3", 1 ] );
splice( @expected_nodes, 8, 1 );
@active_nodes = $graph->active_nodes( @off );
subtest 'Turned on a singleton node' => \&compare_active;
$string = '# when ... with his showers sweet with ... fruit the drought of ... has pierced ... the rood #';
is( make_text( @active_nodes ), $string, "Got the right text" );

@off = $graph->toggle_node( 'node_3' );
splice( @expected_nodes, 3, 1, [ "node_3", 0 ] );
@active_nodes = $graph->active_nodes( @off );
subtest 'Turned off a singleton node' => \&compare_active;
$string = '# when ... showers sweet with ... fruit the drought of ... has pierced ... the rood #';
is( make_text( @active_nodes ), $string, "Got the right text" );

done_testing();
