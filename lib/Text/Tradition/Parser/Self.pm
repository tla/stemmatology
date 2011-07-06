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

my( $IDKEY, $TOKENKEY, $TRANSPOS_KEY, $POSITION_KEY, $CLASS_KEY ) 
    = qw/ name reading identical position class /;

sub parse {
    my( $tradition, $graphml_str ) = @_;
    $DB::single = 1;
    my $graph_data = Text::Tradition::Parser::GraphML::parse( $graphml_str );

    my $collation = $tradition->collation;
    my %witnesses;

    # Add the nodes to the graph. 

    my $extra_data = {}; # Keep track of data that needs to be processed
                         # after the nodes & edges are created.
    print STDERR "Adding graph nodes\n";
    foreach my $n ( @{$graph_data->{'nodes'}} ) {
	# Each node is either a segment or a reading, depending on
	# its class.  Readings have text, segments don't.
	my %node_data = %$n;
	my $nodeid = delete $node_data{$IDKEY};
	my $reading = delete $node_data{$TOKENKEY};
	my $class = $node_data{$CLASS_KEY} || '';
	# TODO this is a hack, fix it?
	$class = 'reading' unless $class eq 'segment';
	my $method = $class eq 'segment' ? "add_$class" : "add_reading";
	my $gnode = $collation->$method( $nodeid );
	$gnode->label( $reading );
	$gnode->set_common if $class eq 'common';

	# Now save the rest of the data, i.e. not the ID or label,
	# if it exists.
	if ( keys %node_data ) {
	    $extra_data->{$nodeid} = \%node_data;
	}
    }
	
    # Now add the edges.
    print STDERR "Adding graph edges\n";
    foreach my $e ( @{$graph_data->{'edges'}} ) {
	my %edge_data = %$e;
	my $from = delete $edge_data{'source'};
	my $to = delete $edge_data{'target'};
	my $class = delete $edge_data{'class'};

	# Whatever is left tells us what kind of edge it is.
	foreach my $wkey ( keys %edge_data ) {
	    if( $wkey =~ /^witness/ ) {
		unless( $class eq 'path' ) {
		    warn "Cannot add witness label to a $class edge";
		    next;
		}
		my $wit = $edge_data{$wkey};
		unless( $witnesses{$wit} ) {
		    $tradition->add_witness( sigil => $wit );
		    $witnesses{$wit} = 1;
		}
		my $label = $wkey eq 'witness_ante_corr' 
		    ? $wit . $collation->ac_label : $wit;
		$collation->add_path( $from->{$IDKEY}, $to->{$IDKEY}, $label );
	    } elsif( $wkey eq 'relationship' ) {
		unless( $class eq 'relationship' ) {
		    warn "Cannot add relationship label to a $class edge";
		    next;
		}
		my $rel = $edge_data{$wkey};
		# TODO handle global relationships
		$collation->add_relationship( $rel, $from->{$IDKEY}, $to->{$IDKEY} );
	    } else {
		my $seg_edge = $collation->graph->add_edge( $from->{$IDKEY}, $to->{$IDKEY} );
		$seg_edge->set_attribute( 'class', 'segment' );
	    }
	}
    }

    ## Deal with node information (transposition, relationships, etc.) that
    ## needs to be processed after all the nodes are created.
    print STDERR "Adding second-pass data\n";
    my $linear = undef;
    foreach my $nkey ( keys %$extra_data ) {
	foreach my $edkey ( keys %{$extra_data->{$nkey}} ) {
	    my $this_reading = $collation->reading( $nkey );
	    if( $edkey eq $TRANSPOS_KEY ) {
		my $other_reading = $collation->reading( $extra_data->{$nkey}->{$edkey} );
		# We evidently have a linear graph.
		$linear = 1;
		$this_reading->set_identical( $other_reading );
	    } elsif ( $edkey eq $POSITION_KEY ) {
		$this_reading->position( $extra_data->{$nkey}->{$edkey} );
	    } else {
		warn "Unfamiliar reading node data $edkey for $nkey";
	    }
	}
    }
    $collation->linear( $linear );

    # We know what the beginning and ending nodes are, no need to
    # search or reset.
    my $end_node = $collation->reading( '#END#' );
    # Walk the paths and make reading sequences for our witnesses.
    # No need to calculate positions as we have them already.
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
