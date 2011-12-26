#!/usr/bin/perl

use strict; use warnings;
use Test::More;
use lib 'lib';
use Text::Tradition;
use Text::Tradition::Stemma;
use XML::LibXML;
use XML::LibXML::XPathContext;

my $datafile = 't/data/Collatex-16.xml'; #TODO need other test data

my $tradition = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'CollateX',
    'file'  => $datafile,
    );
my $stemma = Text::Tradition::Stemma->new( 'collation' => $tradition->collation );

# Test for object creation
ok( $stemma->isa( 'Text::Tradition::Stemma' ), 'Got the right sort of object' );

# Test for character matrix creation
$stemma->make_character_matrix();
 ## check number of rows
 ## check number of columns

# Test that pars runs
my( $status, $tree ) = $stemma->run_phylip_pars();
ok( $status, "pars ran successfully" );
print STDERR "Error was $tree\n" unless $status;

# Test that we get a tree

# Test that the tree has all our witnesses

done_testing();