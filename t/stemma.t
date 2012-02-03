#!/usr/bin/perl

use strict; use warnings;
use File::Which;
use Test::More;
use lib 'lib';
use Text::Tradition;
use Text::Tradition::StemmaUtil;
use XML::LibXML;
use XML::LibXML::XPathContext;

my $datafile = 't/data/Collatex-16.xml'; #TODO need other test data

my $tradition = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'CollateX',
    'file'  => $datafile,
    );
# Set up some relationships
my $c = $tradition->collation;
$c->add_relationship( 'n25', 'n26', { 'type' => 'spelling' } );
$c->add_relationship( 'n9', 'n23', { 'type' => 'spelling' } );
$c->add_relationship( 'n8', 'n13', { 'type' => 'spelling' } );
$c->calculate_ranks();

my $stemma = $tradition->add_stemma( dotfile => 't/data/simple.dot' );

# Test for object creation
ok( $stemma->isa( 'Text::Tradition::Stemma' ), 'Got the right sort of object' );
is( $stemma->graph, '1-2,1-A,2-B,2-C', "Got the correct graph" );

# Test for character matrix creation
my $m = Text::Tradition::StemmaUtil::_make_character_matrix( $c->make_alignment_table() );
 ## check number of rows
is( scalar @$m, 3, "Found three witnesses in char matrix" );
 ## check number of columns
is( scalar( @{$m->[0]} ), 19, "Found 18 rows plus sigla in char matrix" );
 ## check matrix
my %expected = (
	'A' => 'AAAAAAAXAAAAAAAAAA',
	'B' => 'AXXXAAAAAABABAABAA',
	'C' => 'AXXXAAAAABAAAAAXBB',
	);
my @wits = map { shift @$_; } @$m;
map { s/\s+//g } @wits;
foreach my $i ( 0 .. $#wits ) {
	my $w = $wits[$i];
	is( join( '', @{$m->[$i]} ), $expected{$w}, "Row for witness $w is correct" );
}

# Test that pars runs
SKIP: {
    skip "pars not in path", 3 unless File::Which::which('pars');
    my( $status, $tree ) = $stemma->run_phylip_pars();
    ok( $status, "pars ran successfully" );
    print STDERR "Error was $tree\n" unless $status;
    
    # Test that we get a tree
    is( scalar @{$stemma->distance_trees}, 1, "Got a single tree" );
    # Test that the tree has all our witnesses
    $tree = $stemma->distance_trees->[0];
    my @leaves = grep { $tree->degree( $_ ) == 1 } $tree->vertices;
    is( scalar @leaves, 3, "All witnesses in the tree" );
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
