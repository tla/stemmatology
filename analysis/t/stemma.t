#!/usr/bin/perl

use strict; use warnings;
use File::Which;
use Test::More;
use lib 'lib';
use Text::Tradition;
use Text::Tradition::StemmaUtil qw/ character_input phylip_pars parse_newick /;
use TryCatch;

my $datafile = 't/data/Collatex-16.xml'; #TODO need other test data

my $tradition = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'CollateX',
    'file'  => $datafile,
    );
# Set up some relationships
my $c = $tradition->collation;
$c->add_relationship( 'n23', 'n24', { 'type' => 'spelling' } );
$c->add_relationship( 'n9', 'n10', { 'type' => 'spelling' } );
$c->add_relationship( 'n12', 'n13', { 'type' => 'spelling' } );
$c->calculate_ranks();

my $stemma = $tradition->add_stemma( dotfile => 't/data/simple.dot' );

# Test for object creation
ok( $stemma->isa( 'Text::Tradition::Stemma' ), 'Got the right sort of object' );
is( $stemma->graph, '1-2,1-A,2-B,2-C', "Got the correct graph" );

# Test for character matrix creation
my $mstr = character_input( $tradition );
 ## check number of rows
my @mlines = split( "\n", $mstr );
my $msig = shift @mlines;
my( $rows, $chars ) = $msig =~ /(\d+)\s+(\d+)/;
is( $rows, 3, "Found three witnesses in char matrix" );
 ## check number of columns
is( $chars, 18, "Found 18 rows plus sigla in char matrix" );
 ## check matrix
my %expected = (
	'A' => 'AAAAAAAXAAAAAAAAAA',
	'B' => 'AXXXAAAAAABABAABAA',
	'C' => 'AXXXAAAAABAAAAAXBB',
	);
foreach my $ml ( @mlines ) {
	my( $wit, $chars ) = split( /\s+/, $ml );
	is( $chars, $expected{$wit}, "Row for witness $wit is correct" );
}

# Test that pars runs
SKIP: {
	skip "pars not in path", 3 unless File::Which::which('pars');
	my $newick = phylip_pars( $mstr );
	ok( $newick, "pars ran successfully" );

	my $trees = parse_newick( $newick );
	# Test that we get a tree
	is( scalar @$trees, 1, "Got a single tree" );
	# Test that the tree has all our witnesses
	my $tree = $trees->[0];
	is( scalar $tree->witnesses, 3, "All witnesses in the tree" );
}

# Test our dot output
my $display = $stemma->as_dot();
ok( $display =~ /digraph/, "Got a dot display graph" );
ok( $display !~ /hypothetical/, "Graph is display rather than edit" );
# Test our editable output
my $editable = $stemma->editable();
ok( $editable =~ /digraph/, "Got a dot edit graph" );
ok( $editable =~ /hypothetical/, "Graph contains an edit class" );
done_testing();
