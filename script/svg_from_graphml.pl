#!/usr/bin/perl

use lib 'lib';
use strict;
use warnings;
use Text::Tradition;

# First: read the base. Make a graph, but also note which
# nodes represent line beginnings.
my $type = 'CollateX'; # either Self or CollateX

open( GRAPH, $ARGV[0] ) or die "Could not read file $ARGV[0]";
my @lines = <GRAPH>;
close GRAPH;
my $graphml_str = join( '', @lines );

my $tradition = Text::Tradition->new(
    $type => $graphml_str,
    'linear' => 1,
    );

print $tradition->collation->as_svg();
print STDERR "DONE\n";
