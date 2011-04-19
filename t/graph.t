#!/usr/bin/perl

use strict; use warnings;
use Test::More;
use lib 'lib';
use Traditions::Graph;
use XML::LibXML;
use XML::LibXML::XPathContext;

my $datafile = 't/data/Collatex-16.xml';

open( GRAPHFILE, $datafile ) or die "Could not open $datafile";
my @lines = <GRAPHFILE>;
close GRAPHFILE;
my $graph = Traditions::Graph->new( 'GraphML' => join( '', @lines ) );

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
my @expected_nodes = map { [ $_, 1 ] } qw/#START# 1 8 12 13 16 19 20 23 27/;
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
is_deeply( $graph->{'identical_nodes'}, $transposed_nodes, "Found the right transpositions" );

# Test turning on a node
my @off = $graph->toggle_node( '24' );
$expected_nodes[ 15 ] = [ "24", 1 ];
@active_nodes = $graph->active_nodes( @off );
subtest 'Turned on node for new location' => \&compare_active;
$string = '# when ... ... showers sweet with ... fruit the ... of ... has pierced ... the root #';
is( make_text( @active_nodes ), $string, "Got the right text" );
 
# Test the toggling effects of same-column
@off = $graph->toggle_node( '26' );
splice( @expected_nodes, 15, 1, ( [ "24", 0 ], [ "26", 1 ] ) );
@active_nodes = $graph->active_nodes( @off );
subtest 'Turned on other node in that location' => \&compare_active;
$string = '# when ... ... showers sweet with ... fruit the ... of ... has pierced ... the rood #';
is( make_text( @active_nodes ), $string, "Got the right text" );

# Test the toggling effects of transposition

@off = $graph->toggle_node( '14' );
# Add the turned on node
$expected_nodes[ 8 ] = [ "14", 1 ];
# Remove the 'off' for the previous node
splice( @expected_nodes, 15, 1 );
@active_nodes = $graph->active_nodes( @off );
subtest 'Turned on transposition node' => \&compare_active;
$string = '# when ... ... showers sweet with ... fruit the drought of ... has pierced ... the rood #';
is( make_text( @active_nodes ), $string, "Got the right text" );

@off = $graph->toggle_node( '18' );
# Toggle on the new node
$expected_nodes[ 10 ] = [ "18", 1 ];
# Toggle off the transposed node
$expected_nodes[ 8 ] = [ "14", undef ];
@active_nodes = $graph->active_nodes( @off );
subtest 'Turned on that node\'s partner' => \&compare_active;
$string = '# when ... ... showers sweet with ... fruit the ... of drought has pierced ... the rood #';
is( make_text( @active_nodes ), $string, "Got the right text" );

@off = $graph->toggle_node( '14' );
# Toggle on the new node
$expected_nodes[ 8 ] = [ "14", 1 ];
# Toggle off the transposed node
$expected_nodes[ 10 ] = [ "18", undef ];
@active_nodes = $graph->active_nodes( @off );
subtest 'Turned on the original node' => \&compare_active;
$string = '# when ... ... showers sweet with ... fruit the drought of ... has pierced ... the rood #';
is( make_text( @active_nodes ), $string, "Got the right text" );

@off = $graph->toggle_node( '15' );
# Toggle on the new node, and off with the old
splice( @expected_nodes, 8, 1, [ "14", 0 ], [ "15", 1 ] );
@active_nodes = $graph->active_nodes( @off );
subtest 'Turned on the colocated node' => \&compare_active;
$string = '# when ... ... showers sweet with ... fruit the march of ... has pierced ... the rood #';
is( make_text( @active_nodes ), $string, "Got the right text" );

@off = $graph->toggle_node( '3' );
# Toggle on the new node
splice( @expected_nodes, 3, 1, [ "3", 1 ] );
# Remove the old toggle-off
splice( @expected_nodes, 8, 1 );
@active_nodes = $graph->active_nodes( @off );
subtest 'Turned on a singleton node' => \&compare_active;
$string = '# when ... with his showers sweet with ... fruit the march of ... has pierced ... the rood #';
is( make_text( @active_nodes ), $string, "Got the right text" );

@off = $graph->toggle_node( '3' );
# Toggle off this node
splice( @expected_nodes, 3, 1, [ "3", 0 ] );
@active_nodes = $graph->active_nodes( @off );
subtest 'Turned off a singleton node' => \&compare_active;
$string = '# when ... showers sweet with ... fruit the march of ... has pierced ... the rood #';
is( make_text( @active_nodes ), $string, "Got the right text" );

@off = $graph->toggle_node( '21' );
splice( @expected_nodes, 13, 1, [ "21", 1 ] );
@active_nodes = $graph->active_nodes( @off );
subtest 'Turned on a new node after singleton switchoff' => \&compare_active;
$string = '# when ... showers sweet with ... fruit the march of ... has pierced unto the rood #';
is( make_text( @active_nodes ), $string, "Got the right text" );

done_testing();
