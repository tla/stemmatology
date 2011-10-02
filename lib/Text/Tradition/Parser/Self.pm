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

# TODO share these with Collation.pm somehow
my( $IDKEY, $TOKENKEY, $TRANSPOS_KEY, $RANK_KEY, $CLASS_KEY,
	$SOURCE_KEY, $TARGET_KEY, $WITNESS_KEY, $EXTRA_KEY, $RELATIONSHIP_KEY ) 
    = qw/ name reading identical rank class 
    	  source target witness extra relationship/;

sub parse {
    my( $tradition, $graphml_str ) = @_;
    
    # TODO this is begging for stream parsing instead of multiple loops.
    my $graph_data = Text::Tradition::Parser::GraphML::parse( $graphml_str );

    my $collation = $tradition->collation;
    my %witnesses;

    # Add the nodes to the graph. 
    # TODO Are we adding extra start/end nodes?

    my $extra_data = {}; # Keep track of data that needs to be processed
                         # after the nodes & edges are created.
    print STDERR "Adding graph nodes\n";
    foreach my $n ( @{$graph_data->{'nodes'}} ) {
    	# First extract the data that we can use without reference to
    	# anything else.
    	my %node_data = %$n; # Need $n itself untouched for edge processing
        my $nodeid = delete $node_data{$IDKEY};
        my $reading = delete $node_data{$TOKENKEY};
        my $class = delete $node_data{$CLASS_KEY} || '';
        my $rank = delete $node_data{$RANK_KEY};
        
        # Create the node.  Current valid classes are common and meta. 
        # Everything else is a normal reading.  
        my $gnode = $collation->add_reading( $nodeid );
        $gnode->text( $reading );
        $gnode->make_common if $class eq 'common';
        $gnode->is_meta( 1 ) if $class eq 'meta';
        $gnode->rank( $rank ) if defined $rank;

        # Now save the data that we need for post-processing,
        # if it exists.
        if ( keys %node_data ) {
            $extra_data->{$nodeid} = \%node_data
        }
    }
        
    # Now add the edges.
    print STDERR "Adding graph edges\n";
    foreach my $e ( @{$graph_data->{'edges'}} ) {
        my $from = $e->{$SOURCE_KEY};
        my $to = $e->{$TARGET_KEY};
        my $class = $e->{$CLASS_KEY};

        # We may have more information depending on the class.
        if( $class eq 'path' ) {
        	# We need the witness, and whether it is an 'extra' reading path.
        	my $wit = $e->{$WITNESS_KEY};
        	warn "No witness label on path edge!" unless $wit;
        	my $extra = $e->{$EXTRA_KEY};
        	my $label = $wit . ( $extra ? $collation->ac_label : '' );
        	$collation->add_path( $from->{$IDKEY}, $to->{$IDKEY}, $label );
        	# Add the witness if we don't have it already.
			unless( $witnesses{$wit} ) {
				$tradition->add_witness( sigil => $wit );
				$witnesses{$wit} = 1;
			}
        } elsif( $class eq 'relationship' ) {
        	# We need the metadata about the relationship.
        	my $opts = { 'type' => $e->{$RELATIONSHIP_KEY} };
        	$opts->{'equal_rank'} = $e->{'equal_rank'} 
        		if exists $e->{'equal_rank'};
        	$opts->{'non_correctable'} = $e->{'non_correctable'} 
        		if exists $e->{'non_correctable'};
        	$opts->{'non_independent'} = $e->{'non_independent'} 
        		if exists $e->{'non_independent'};
        	warn "No relationship type for relationship edge!" unless $opts->{'type'};
        	$collation->add_relationship( $from->{$IDKEY}, $to->{$IDKEY}, $opts );
        } 
    }

    ## Deal with node information (transposition, relationships, etc.) that
    ## needs to be processed after all the nodes are created.
    print STDERR "Adding second-pass node data\n";
    my $linear = undef;
    foreach my $nkey ( keys %$extra_data ) {
        foreach my $edkey ( keys %{$extra_data->{$nkey}} ) {
            my $this_reading = $collation->reading( $nkey );
            if( $edkey eq $TRANSPOS_KEY ) {
                my $other_reading = $collation->reading( $extra_data->{$nkey}->{$edkey} );
                # We evidently have a linear graph.
                $linear = 1;
                $this_reading->set_identical( $other_reading );
            } else {
                warn "Unfamiliar reading node data $edkey for $nkey";
            }
        }
    }
    $collation->linear( $linear );
    # TODO We probably need to set the $witness->path arrays for each wit.
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
