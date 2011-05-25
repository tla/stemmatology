#!/usr/bin/env perl

use lib 'lib';
use strict;
use warnings;
use Text::Tradition;

my $tradition = Text::Tradition->new( 
				      'CSV' => $ARGV[0],
				      'base' => $ARGV[1],
				      );

print $tradition->collation->as_svg();
print STDERR "Done\n";
