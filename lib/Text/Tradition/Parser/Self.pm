package Text::Tradition::Parser::Self;

use strict;
use warnings;
use Text::Tradition::Parser::GraphML;

=head1 NAME

Text::Tradition::Parser::GraphML

=head1 DESCRIPTION

Parser module for Text::Tradition to read in its own GraphML output format.
TODO document what this format is.

=head1 METHODS

=over

=item B<parse>

parse( $graph, $graphml_string );

Takes an initialized Text::Tradition::Graph object and a string
containing the GraphML; creates the appropriate nodes and edges on the
graph.

=cut

my( $IDKEY, $TOKENKEY, $TRANSPOS_KEY, $POSITION_KEY ) 
    = qw/ name reading identical position /;

sub parse {
    my( $tradition, $graphml_str ) = @_;
    my $graph_data = Text::Tradition::Parser::GraphML::parse( $graphml_str );

    my $collation = $tradition->collation;
    my %witnesses;

    # Add the nodes to the graph. 

    my $extra_data = {}; # Keep track of data that needs to be processed
                         # after the nodes & edges are created.
    foreach my $n ( @{$graph_data->{'nodes'}} ) {
	# Could use a better way of registering these
	my %node_data = %$n;
	my $nodeid = delete $node_data{$IDKEY};
	my $reading = delete $node_data{$TOKENKEY};
	my $gnode = $collation->add_reading( $nodeid );
	$gnode->text( $reading );

	# Now save the rest of the data, i.e. not the ID or label,
	# if it exists.
	if ( keys %node_data ) {
	    $extra_data->{$nodeid} = \%node_data;
	}
    }
	
    # Now add the edges.
    foreach my $e ( @{$graph_data->{'edges'}} ) {
	my %edge_data = %$e;
	my $from = delete $edge_data{'source'};
	my $to = delete $edge_data{'target'};

	# Whatever is left tells us what kind of edge it is.
	foreach my $wkey ( keys %edge_data ) {
	    if( $wkey =~ /^witness/ ) {
		my $wit = $edge_data{$wkey};
		unless( $witnesses{$wit} ) {
		    $tradition->add_witness( sigil => $wit );
		    $witnesses{$wit} = 1;
		}
		my $label = $wkey eq 'witness_ante_corr' 
		    ? $wit . $collation->ac_label : $wit;
		$collation->add_path( $from->{$IDKEY}, $to->{$IDKEY}, $label );
	    } else {
		my $rel = $edge_data{$wkey};
		# TODO handle global relationships
		$collation->add_relationship( $rel, $from->{$IDKEY}, $to->{$IDKEY} );
	    }
	}
    }

    ## Deal with node information (transposition, relationships, etc.) that
    ## needs to be processed after all the nodes are created.
    foreach my $nkey ( keys %$extra_data ) {
	foreach my $edkey ( keys %{$extra_data->{$nkey}} ) {
	    my $this_reading = $collation->reading( $nkey );
	    if( $edkey eq $TRANSPOS_KEY ) {
		my $other_reading = $collation->reading( $extra_data->{$nkey}->{$edkey} );
		if( $collation->linear ) {
		    $this_reading->set_identical( $other_reading );
		} else {
		    $collation->merge_readings( $other_reading, $this_reading );
		}
	    } elsif ( $edkey eq $POSITION_KEY ) {
		$this_reading->position( $extra_data->{$nkey}->{$edkey} );
	    } else {
		warn "Unfamiliar reading node data $edkey for $nkey";
	    }
	}
    }

    # We know what the beginning and ending nodes are, no need to
    # search or reset.
    my $end_node = $collation->reading( '#END#' );
    $DB::single = 1;
    # Walk the paths and make reading sequences for our witnesses.
    $collation->walk_witness_paths( $end_node );
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
