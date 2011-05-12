#!/usr/bin/env perl

use lib 'lib';
use strict;
use warnings;
use Text::Tradition::Graph;

my $collation_graph = Text::Tradition::Graph->new( 
						   'CSV' => $ARGV[0],
						   'base' => $ARGV[1],
						   );

print $collation_graph->as_svg();
print STDERR "Done\n";
