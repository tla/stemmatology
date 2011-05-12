#!/usr/bin/perl

use lib 'lib';
use strict;
use warnings;
use Text::Tradition::Graph;

# First: read the base. Make a graph, but also note which
# nodes represent line beginnings.

open( GRAPH, $ARGV[0] ) or die "Could not read file $ARGV[0]";
my @lines = <GRAPH>;
close GRAPH;
my $graphml_str = join( '', @lines );

my $collation_graph = Text::Tradition::Graph->new(
    'GraphML' => $graphml_str,
    );

print $collation_graph->as_svg();
print STDERR "DONE\n";
