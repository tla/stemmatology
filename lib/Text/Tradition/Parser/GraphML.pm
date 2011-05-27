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

use vars qw/ $xpc %nodedata /;

sub parse {
    my( $tradition, $graphml_str ) = @_;

    my $collation = $tradition->collation;
    my $parser = XML::LibXML->new();
    my $doc = $parser->parse_string( $graphml_str );
    my $graphml = $doc->documentElement();
    $xpc = XML::LibXML::XPathContext->new( $graphml );
    $xpc->registerNs( 'g', 'http://graphml.graphdrawing.org/xmlns' );
    
    # First get the ID keys, for witnesses and for collation data
    my %witnesses;
    foreach my $k ( $xpc->findnodes( '//g:key' ) ) {
	# Each key has a 'for' attribute; the edge keys are witnesses, and
	# the node keys contain an ID and string for each node.

	if( $k->getAttribute( 'for' ) eq 'node' ) {
	    # The node data keys we expect are:
	    # 'number|name' -> unique node identifier
	    # 'token|reading' -> reading for the node
	    # 'identical' -> the node of which this node is 
	    #                a transposed version
	    # 'position' -> a calculated position for the node
	    $nodedata{ $k->getAttribute( 'attr.name' ) } = $k->getAttribute( 'id' );
	} else {
	    $witnesses{ $k->getAttribute( 'id' ) } = $k->getAttribute( 'attr.name' );
	}
    }

    my $has_explicit_positions = defined $nodedata{'position'};

    # Add the witnesses that we have found
    foreach my $wit ( values %witnesses ) {
	$tradition->add_witness( 'sigil' => $wit );
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
	my $id = _lookup_node_data( $n, 'number' );
	$id = _lookup_node_data( $n, 'name' ) unless $id;
	my $token = _lookup_node_data( $n, 'token' );
	$token = _lookup_node_data( $n, 'reading' ) unless $token;
	my $gnode = $collation->add_reading( $id );
	$node_name{ $n->getAttribute('id') } = $id;
	$gnode->text( $token );

	# Now get the rest of the data, i.e. not the ID or label
	my $extra = {};
	foreach my $k ( keys %nodedata ) {
	    next if $k =~ /^(number|token)$/;
	    $extra->{ $k } = _lookup_node_data( $n, $k );
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
	# One path per witness
	foreach( @wit_names ) {
	    $collation->add_path( $from, $to, $_ );
	}
	# Only a single path between two readings
	# my $label = $collation->path_label( @wit_names );
	# $collation->add_path( $from, $to, $label );
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
	# print STDERR "Checking node " . $gnode->name . "\n";
	my @outgoing = $gnode->outgoing();
	my @incoming = $gnode->incoming();

	unless( scalar @incoming ) {
	    warn "Already have a beginning node" if $begin_node;
	    $begin_node = $gnode;
	    $collation->start( $gnode );
	}
	unless( scalar @outgoing ) {
	    warn "Already have an ending node" if $end_node;
	    $end_node = $gnode;
	}
    }

    my @common_nodes = $collation->walk_witness_paths( $end_node );
    # Now we have added the witnesses and their paths, so have also
    # implicitly marked the common nodes. Now we can calculate their
    # explicit permissions.  This is separate because it won't always
    # be necessary with the GraphML parsing.
    if( $has_explicit_positions ) {
	# Record the positions that came with each graph node.
	# TODO we really need to translate these into our own style of
	# position identifier.  That's why we defer this until now.
	foreach my $node_id ( keys %$extra_data ) {
	    my $pos = $extra_data->{$node_id}->{'position'};
	    $collation->reading( $node_name{$node_id} )->position( $pos );
	}
    } else {
	# Calculate a position for each graph node.
	$collation->calculate_positions( @common_nodes );
    }
}

sub _lookup_node_data {
    my( $xmlnode, $key ) = @_;
    return undef unless exists $nodedata{$key};
    my $lookup_xpath = './g:data[@key="%s"]/child::text()';
    my $data = $xpc->findvalue( sprintf( $lookup_xpath, $nodedata{$key} ), 
				$xmlnode );
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
