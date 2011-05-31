package Text::Tradition::Parser::CollateX;

use strict;
use warnings;
use Text::Tradition::Parser::GraphML;

=head1 NAME

Text::Tradition::Parser::CollateX

=head1 DESCRIPTION

Parser module for Text::Tradition, given a GraphML file from the
CollateX program that describes a collation graph.  For further
information on the GraphML format for text collation, see
http://gregor.middell.net/collatex/

=head1 METHODS

=over

=item B<parse>

parse( $graph, $graphml_string );

Takes an initialized Text::Tradition::Graph object and a string
containing the GraphML; creates the appropriate nodes and edges on the
graph.

=cut

my $IDKEY = 'number';
my $CONTENTKEY = 'token';
my $TRANSKEY = 'identical';

sub parse {
    my( $tradition, $graphml_str ) = @_;
    my $graph_data = Text::Tradition::Parser::GraphML::parse( $graphml_str );
    my $collation = $tradition->collation;
    my %witnesses; # Keep track of the witnesses we encounter as we
                   # run through the graph data.

    # Add the nodes to the graph.  First delete the start node, because
    # GraphML graphs will have their own start nodes.
    $collation->del_reading( $collation->start() );

    my $extra_data = {}; # Keep track of info to be processed after all
                         # nodes have been created
    foreach my $n ( @{$graph_data->{'nodes'}} ) {
	my %node_data = %$n;
	my $nodeid = delete $node_data{$IDKEY};
	my $token = delete $node_data{$CONTENTKEY};
	unless( $nodeid && $token ) {
	    warn "Did not find an ID or token for graph node, can't add it";
	    next;
	}
	my $gnode = $collation->add_reading( $nodeid );
	$gnode->text( $token );

	# Whatever is left is extra info to be processed later.
	if( keys %node_data ) {
	    $extra_data->{$nodeid} = \%node_data;
	}
    }
	
    # Now add the edges.
    foreach my $e ( @{$graph_data->{'edges'}} ) {
	my %edge_data = %$e;
	my $from = delete $edge_data{'source'};
	my $to = delete $edge_data{'target'};

	# In CollateX, we have a distinct witness data ID per witness,
	# so that we can have multiple witnesses per edge.  We want to
	# translate this to one witness per edge in our own
	# representation.
	foreach my $ekey ( keys %edge_data ) {
	    my $wit = $edge_data{$ekey};
	    # Create the witness object if it does not yet exist.
	    unless( $witnesses{$wit} ) {
		$tradition->add_witness( 'sigil' => $wit );
		$witnesses{$wit} = 1;
	    }
	    $collation->add_path( $from->{$IDKEY}, $to->{$IDKEY}, $wit );
	}
    }

    # Process the extra node data if it exists.
    foreach my $nodeid ( keys %$extra_data ) {
	my $ed = $extra_data->{$nodeid};
	if( exists $ed->{$TRANSKEY} ) {
	    
	    my $tn_reading = $collation->reading( $nodeid );
	    my $main_reading = $collation->reading( $ed->{$TRANSKEY} );
	    if( $collation->linear ) {
		$tn_reading->set_identical( $main_reading );
	    } else {
		$collation->merge_readings( $main_reading, $tn_reading );
	    }
	} # else we don't have any other tags to process yet.
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

    # Record for each witness its sequence of readings, and determine
    # the common nodes.
    my @common_nodes = $collation->walk_witness_paths( $end_node );

    # Now we have added the witnesses and their paths, so have also
    # implicitly marked the common nodes. Now we can calculate their
    # explicit positions.
    $collation->calculate_positions( @common_nodes );
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
