package Text::Tradition::Parser::GraphML;

use strict;
use warnings;
use XML::LibXML;
use XML::LibXML::XPathContext;

=head1 NAME

Text::Tradition::Parser::GraphML

=head1 DESCRIPTION

Parser module for Text::Tradition, given a GraphML file that describes
a collation graph.  Returns the information about the graph that has
been parsed out from the GraphML.  This module is meant to be used
with a module (e.g. CollateX or Self) that interprets the specific
GraphML conventions of the source program.

=head1 METHODS

=over

=item B<parse>

parse( $graphml_string );

Takes a string containing the GraphML; returns a list of nodes, edges,
and their associated data.

=cut

use vars qw/ $xpc $nodedata $witnesses /;

# Return graph -> nodeid -> { key1/val1, key2/val2, key3/val3 ... }
#              -> edgeid -> { source, target, wit1/val1, wit2/val2 ...}

sub parse {
    my( $graphml_str ) = @_;

    my $graph_hash = { 'nodes' => [],
		       'edges' => [] };

    my $parser = XML::LibXML->new();
    my $doc = $parser->parse_string( $graphml_str );
    my $graphml = $doc->documentElement();
    $xpc = XML::LibXML::XPathContext->new( $graphml );
    $xpc->registerNs( 'g', 'http://graphml.graphdrawing.org/xmlns' );
    
    # First get the ID keys, for witnesses and for collation data
    foreach my $k ( $xpc->findnodes( '//g:key' ) ) {
	# Each key has a 'for' attribute; the edge keys are witnesses, and
	# the node keys contain an ID and string for each node.
	my $keyid = $k->getAttribute( 'id' );
	my $keyname = $k->getAttribute( 'attr.name' );

	if( $k->getAttribute( 'for' ) eq 'node' ) {
	    # Keep track of the XML identifiers for the data carried
	    # in each node element.
	    $nodedata->{$keyid} = $keyname
	} else {
	    $witnesses->{$keyid} = $keyname;
	}
    }

    my $graph_el = $xpc->find( '/g:graphml/g:graph' )->[0];

    my $node_reg = {};

    # Add the nodes to the graph hash. 
    my @nodes = $xpc->findnodes( '//g:node' );
    foreach my $n ( @nodes ) {
	# Could use a better way of registering these
	my $node_hash = {};
	foreach my $dkey ( keys %$nodedata ) {
	    my $keyname = $nodedata->{$dkey};
	    my $keyvalue = _lookup_node_data( $n, $dkey );
	    $node_hash->{$keyname} = $keyvalue if $keyvalue;
	}
	$node_reg->{$n->getAttribute( 'id' )} = $node_hash;
	push( @{$graph_hash->{'nodes'}}, $node_hash );
    }
	
    # Now add the edges, and cross-ref with the node objects.
    my @edges = $xpc->findnodes( '//g:edge' );
    foreach my $e ( @edges ) {
	my $from = $e->getAttribute('source');
	my $to = $e->getAttribute('target');

	# We don't know whether the edge data is one per witness
	# or one per witness type, or something else.  So we just
	# save it and let our calling parser decide.
	my $edge_hash = {
	    'source' => $node_reg->{$from},
	    'target' => $node_reg->{$to},
	};
	foreach my $wkey( keys %$witnesses ) {
	    my $wname = $witnesses->{$wkey};
	    my $wlabel = _lookup_node_data( $e, $wkey );
	    $edge_hash->{$wname} = $wlabel if $wlabel;
	}
	push( @{$graph_hash->{'edges'}}, $edge_hash );
    }
    return $graph_hash;
}

sub _lookup_node_data {
    my( $xmlnode, $key ) = @_;
    my $lookup_xpath = './g:data[@key="%s"]/child::text()';
    my $data = $xpc->findvalue( sprintf( $lookup_xpath, $key ), $xmlnode );
    return $data;
}
    
=back

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews, aurum@cpan.org

=cut

1;
