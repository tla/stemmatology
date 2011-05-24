#!/usr/bin/perl

use lib 'lib';
use strict;
use warnings;
use Text::Tradition;

# First: read the base. Make a graph, but also note which
# nodes represent line beginnings.

my $tradition = Text::Tradition->new(
    'CSV' => $ARGV[0],
    'base' => $ARGV[1],
    );


print $tradition->collation->as_svg();
print STDERR "DONE\n";
__END__
my $rows = 0;
my $matrix = [];
foreach my $pos ( $collation_graph->{'positions'}->all ) {
    my @p_nodes = $collation_graph->{'positions'}->nodes_at_position( $pos );
    $rows = scalar @p_nodes
	if $rows < scalar @p_nodes;
    push( @$matrix, \@p_nodes );
}
print "<html><head><title>A table</title></head><body><table>\n";
foreach my $i ( 0 .. $rows-1 ) {
    print "\t<tr>\n";
    foreach my $col( @$matrix ) {
	my $str = '';
	if( $col->[$i] ) {
	    $str = $collation_graph->node( $col->[$i] )->label;
	}
	printf( "\t\t<td>%s</td>\n", $str );
    }
    print "\t</tr>\n";
}
print "</table></body></html>\n";
    

