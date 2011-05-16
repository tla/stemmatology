package Text::Tradition::Parser::GraphML;

use strict;
use warnings;
use XML::LibXML;
use XML::LibXML::XPathContext;

=head1 NAME

Text::Tradition::Parser::GraphML

=head1 DESCRIPTION

Parser module for Text::Tradition, given a GraphML file that describes
a collation graph.  For further information on the GraphML format for
text collation, see http://gregor.middell.net/collatex/

=head1 METHODS

=over

=item B<parse>

parse( $graph, $graphml_string );

Takes an initialized Text::Tradition::Graph object and a string
containing the GraphML; creates the appropriate nodes and edges on the
graph.

=cut

sub parse {
    my( $collation, $graphml_str ) = @_;

    my $parser = XML::LibXML->new();
    my $doc = $parser->parse_string( $graphml_str );
    my $graphml = $doc->documentElement();
    my $xpc = XML::LibXML::XPathContext->new( $graphml );
    $xpc->registerNs( 'g', 'http://graphml.graphdrawing.org/xmlns' );
    
    # First get the ID keys, for witnesses and for collation data
    my %nodedata;
    my %witnesses;
    foreach my $k ( $xpc->findnodes( '//g:key' ) ) {
	# Each key has a 'for' attribute; the edge keys are witnesses, and
	# the node keys contain an ID and string for each node.

	if( $k->getAttribute( 'for' ) eq 'node' ) {
	    $nodedata{ $k->getAttribute( 'attr.name' ) } = $k->getAttribute( 'id' );
	} else {
	    $witnesses{ $k->getAttribute( 'id' ) } = $k->getAttribute( 'attr.name' );
	}
    }

    my $graph_el = $xpc->find( '/g:graphml/g:graph' )->[0];

    # Add the nodes to the graph.  First delete the start node, because
    # GraphML graphs will have their own start nodes.
    $collation->del_reading( $collation->start() );
    # Map from XML IDs to node name/identity
    my %node_name;
    # Keep track of whatever extra info we're passed
    my $extra_data = {};
    my @nodes = $xpc->findnodes( '//g:node' );
    foreach my $n ( @nodes ) {
	my $lookup_xpath = './g:data[@key="%s"]/child::text()';
	my $id = $xpc->findvalue( sprintf( $lookup_xpath, $nodedata{'number'} ), $n );
	my $label = $xpc->findvalue( sprintf( $lookup_xpath, $nodedata{'token'} ), $n );
	my $gnode = $collation->add_reading( $id );
	$node_name{ $n->getAttribute('id') } = $id;
	$gnode->set_attribute( 'label', $label );

	# Now get the rest of the data
	my $extra = {};
	my @keys = grep { $_ !~ /^(number|token)$/ } keys( %nodedata );
	foreach my $k ( @keys ) {
	    my $data = $xpc->findvalue( sprintf( $lookup_xpath, $nodedata{ $k } ), $n );
	    $extra->{ $k } = $data;
	}
	$extra_data->{ $id } = $extra;
    }
	
    # Now add the edges.
    my @edges = $xpc->findnodes( '//g:edge' );
    foreach my $e ( @edges ) {
	my $from = $node_name{ $e->getAttribute('source') };
	my $to = $node_name{ $e->getAttribute('target') };
	# Label according to the witnesses present.
	my @wit_ids = $xpc->findnodes( './g:data/attribute::key', $e );
	my @wit_names = map { $witnesses{ $_->getValue() } } @wit_ids;
	my $label = join( ', ', @wit_names );
	    
	$collation->add_path( $from, $to, $label );
    }

    ## Reverse the node_name hash so that we have two-way lookup.
    my %node_id = reverse %node_name;

    ## Record the nodes that are marked as transposed.
    my $tr_xpath = '//g:node[g:data[@key="' . $nodedata{'identical'} . '"]]';
    my $transposition_nodes = $xpc->find( $tr_xpath );
    foreach my $tn ( @$transposition_nodes ) {
	my $id_xpath = sprintf( './g:data[@key="%s"]/text()', 
				$nodedata{'identical'} );
	$collation->reading( $node_id{ $tn->getAttribute( 'id' ) } )->
	    set_identical( $collation->reading( 
			       $node_name{ $xpc->findvalue( $id_xpath, $tn ) } ) );
    }


    # Find the beginning and end nodes of the graph.  The beginning node
    # has no incoming edges; the end node has no outgoing edges.
    my( $begin_node, $end_node );
    foreach my $gnode ( $collation->readings() ) {
	print STDERR "Checking node " . $gnode->name . "\n";
	my @outgoing = $gnode->outgoing();
	my @incoming = $gnode->incoming();

	unless( scalar @incoming ) {
	    warn "Already have a beginning node" if $begin_node;
	    my $node_xml_id = $node_id{ $gnode->name() };
	    my @bn = $xpc->findnodes( '//g:node[@id="' . $node_xml_id . '"]' );
	    warn "XPath did not find a node for id $node_xml_id"
		unless scalar @bn;
	    $begin_node = $bn[0];
	    $collation->start( $gnode );
	    $node_name{ $begin_node->getAttribute( 'id' ) } = '#START#';
	    $node_id{'#START#'} = $begin_node->getAttribute( 'id' );
	}
	unless( scalar @outgoing ) {
	    warn "Already have an ending node" if $end_node;
	    my $node_xml_id = $node_id{ $gnode->name() };
	    my @bn = $xpc->findnodes( '//g:node[@id="' . $node_xml_id . '"]' );
	    warn "XPath did not find a node for id $node_xml_id"
		unless scalar @bn;
	    $end_node = $bn[0];
	}
    }

    # Now for each witness, walk the path through the graph.
    # Then we need to find the common nodes.  
    # TODO This method is going to fall down if we have a very gappy 
    # text in the collation.
    # TODO think about whether it makes more sense to do this in the
    # XML or in the graph. Right now it's the XML.
    my $paths = {};
    my @common_nodes;
    foreach my $wit ( keys %witnesses ) {
	my $node_id = $begin_node->getAttribute('id');
	my @wit_path = ( $node_name{ $node_id } );
	# TODO Detect loops at some point
	while( $node_id ne $end_node->getAttribute('id') ) {
	    # Find the node which is the target of the edge whose
	    # source is $node_id and applies to this witness.
	    my $xpath_expr = '//g:edge[child::g:data[@key="' 
		. $wit . '"] and attribute::source="'
		. $node_id . '"]';
	    my $next_edge = $xpc->find( $xpath_expr, $graph_el )->[0];
	    print STDERR " - at $wit / $node_id\n";
	    $node_id = $next_edge->getAttribute('target');
	    push( @wit_path, $node_name{ $node_id } );
	}
	$paths->{ $witnesses{ $wit }} = \@wit_path;
	if( @common_nodes ) {
	    my @cn;
	    foreach my $n ( @wit_path) {
		push( @cn, $n ) if grep { $_ eq $n } @common_nodes;
	    }
	    @common_nodes = ();
	    push( @common_nodes, @cn );
	} else {
	    push( @common_nodes, @wit_path );
	}
    }

    # Mark all the nodes as either common or not.
    foreach my $cn ( @common_nodes ) {
	print STDERR "Setting $cn as common node\n";
	$collation->reading( $cn )->set_attribute( 'class', 'common' );
    }
    foreach my $n ( $collation->readings() ) {
	$n->set_attribute( 'class', 'variant' )
	    unless $n->get_attribute( 'class' ) eq 'common';
    }

    # Now calculate graph positions.
    # $collation->make_positions( \@common_nodes, $paths );

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
