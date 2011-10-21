package Text::Tradition::Analysis;

use strict;
use warnings;
use Text::Tradition;
use Text::Tradition::Stemma;

sub new {
	my( $class, $args ) = @_;
	my $self = {};
	bless( $self, $class );	
	$self->{'data'} = [];
	foreach my $t ( @{$args->{'traditions'}} ) {
	    $self->run_analysis( $t->{'file'}, $t->{'stemmadot'} );
	}
	return $self;
}

sub run_analysis {
	my( $self, $file, $stemmadot ) = @_;
	# What we will return
	my $svg;
	my $variants = [];
	my $data = {};
	
	# Read in the file and stemma	
	my $tradition = Text::Tradition->new( 
		'input'  => 'Self',
		'file'   => $file,
		'linear' => 1,
		);
	$data->{'title'} = $tradition->name;
	
	my $stemma = Text::Tradition::Stemma->new(
		'collation' => $tradition->collation,
		'dot' => $stemmadot,
		);
	# We will return the stemma picture
	$svg = $stemma->as_svg( { size => "8,7.5" } );;
	$data->{'svg'} = $svg;
	
	# We have the collation, so get the alignment table with witnesses in rows.
	# Also return the reading objects in the table, rather than just the words.
	my $wits = {};
	map { $wits->{$_} = 1 } $stemma->witnesses;
	my $all_wits_table = $tradition->collation->make_alignment_table( 'refs', $wits );
	
	# For each column in the alignment table, we want to see if the existing
	# groupings of witnesses match our stemma hypothesis. We also want, at the
	# end, to produce an HTML table with all the variants.
	my $html_columns = 0;
	my ( $total, $genealogical, $conflicts ) = ( 0, 0, 0 );
	
	# Strip the list of sigla and save it for correlation to the readings.
	my $col_wits = shift @$all_wits_table;
	
	# We will return a data structure, an array for each row that looks like:
	# { id = X, genealogical = Y, readings = [ text = X, group = Y], empty = N }
	foreach my $i ( 0 .. $#$all_wits_table ) {
		# For each column in the table, group the readings by witness.
		my $rdg_wits = {};
		my $col_rdgs = shift @$all_wits_table;
		my $rank;
		my $lacunose = [];
		foreach my $j ( 0 .. $#{$col_rdgs} ) {
			my $rdg = $col_rdgs->[$j];
			my $rdg_text = '(omitted)';  # Initialize in case of empty reading
			if( $rdg ) {
			    if( $rdg->is_lacuna ) {
			        $rdg_text = undef;   # Don't count lacunae
			        push( @$lacunose, $col_wits->[$j] );
			    } else {
    				$rdg_text = $rdg->text; 
				    # Get the rank from any real reading; they should be identical.
				    $rank = $rdg->rank;
				}
			}
			if( defined $rdg_text ) {
				# Initialize the witness array if we haven't got one yet
				$rdg_wits->{$rdg_text} = [] unless $rdg_wits->{$rdg_text};
				# Add the relevant witness, subject to a.c. logic
				add_variant_wit( $rdg_wits->{$rdg_text}, $col_wits->[$j],
					$tradition->collation->ac_label );
			}
		}
		
		# See if this column has any potentially genealogical variants.
		# If not, skip to the next.
		$total++ unless scalar keys %$rdg_wits == 1;
		my( $groups, $readings ) = useful_variant( $rdg_wits );
		next unless $groups && $readings;  
		
		# Keep track of our widest row
		$html_columns = scalar @$groups if scalar @$groups > $html_columns;
		
		# We can already look up witnesses for a reading; we also want to look
		# up readings for a given witness.
		my $group_readings = {};
		foreach my $x ( 0 .. $#$groups ) {
			$group_readings->{wit_stringify( $groups->[$x] )} = $readings->[$x];
		}
		
		# For all the groups with more than one member, collect the list of all
		# contiguous vertices needed to connect them.
		$DB::single = 1;
		my $variant_row = analyze_variant_location( $group_readings, $groups, 
		    $stemma->graph, $lacunose );
		$variant_row->{'id'} = $rank;
		$genealogical++ if $variant_row->{'genealogical'};
		$conflicts += grep { $_->{'conflict'} } @{$variant_row->{'readings'}};

		# Now run the same analysis given the calculated distance tree(s).
# 		my @trees = @{$stemma->distance_trees};
# 		if( @trees ) {
#             foreach my $tree ( 0 .. $#trees ) {
#                 my $dc = analyze_variant_location( $group_readings, $groups, $tree, $lacunose, 'undirected' );
#                 foreach my $rdg ( keys %$dc ) {
#                     my $var = $dc->{$rdg};
#                     # TODO Do something with this
#                 }
#             }
# 	    }

		# Record that we used this variant in an analysis
		push( @$variants, $variant_row );
	}
	
	# Go through our variant rows, after we have seen all of them once,
	# and add the number of empty columns needed by each.
	foreach my $row ( @$variants ) {
		my $empty = $html_columns - scalar @{$row->{'readings'}};
		$row->{'empty'} = $empty;
	}
	
	# Populate self with our analysis data.
	$data->{'variants'} = $variants;
	$data->{'variant_count'} = $total;
	$data->{'conflict_count'} = $conflicts;
	$data->{'genealogical_count'} = $genealogical;
	push( @{$self->{'data'}}, $data );
}

# variant_row -> genealogical
#             -> readings [ { text, group, conflict, missing } ]

sub analyze_variant_location {
    my( $group_readings, $groups, $graph, $lacunose, $undirected ) = @_;
    my $contig = {};
    my $subgraph = {};
    my $is_conflicted;
    my $conflict = {};
    my $missing = {};
    map { $missing->{$_} = 1 } @$lacunose;
    my $variant_row = { 'readings' => [] };
    # Mark each ms as in its own group, first.
    foreach my $g ( @$groups ) {
        my $gst = wit_stringify( $g );
        map { $contig->{$_} = $gst } @$g;
    }
    # Now for each unmarked node in the graph, initialize an array
    # for possible group memberships.  We will use this later to
    # resolve potential conflicts.
    map { $contig->{$_} = [] unless $contig->{$_} } $graph->vertices;
    foreach my $g ( sort { scalar @$b <=> scalar @$a } @$groups ) {
        my $gst = wit_stringify( $g );  # This is the group name
        my $reachable = { $g->[0] => 1 };
        # Copy the graph, and delete all non-members from the new graph.
        my $part = $graph->copy;
        my $group_root;
        $part->delete_vertices( 
            grep { !ref( $contig->{$_} ) && $contig->{$_} ne $gst } $graph->vertices );
                
        # Now look to see if our group is connected.
        if( $undirected ) { # For use with distance trees etc.
            # Find all vertices reachable from the first (arbitrary) group
            # member.  If we are genealogical this should include them all. 
            map { $reachable->{$_} = 1 } $part->all_reachable( $g->[0] );
            # TODO This is a terrible way to do distance trees, since all
            # non-leaf nodes are included in every graph part now. We may
            # have to go back to SPDP.
        } else {
            if( @$g > 1 ) {
                # Dispense with the trivial case of one reading.
                # We have to take directionality into account.
                # How many root nodes do we have?
                my @roots = grep { ref( $contig->{$_} ) || $contig->{$_} eq $gst } 
                    $part->source_vertices;
                # Assuming that @$g > 1, find the first root node that has at
                # least one successor belonging to our group. If this reading
                # is genealogical, there should be only one, but we will check
                # that implicitly later.
                my $nodes_in_subtree = 0;
                foreach my $root ( @roots ) {
                    # Prune the tree to get rid of extraneous hypotheticals.
                    $root = prune_subtree( $part, $root, $contig );
                    # Get all the successor nodes of our root.
                    my $tmp_reach = { $root => 1 };
                    map { $tmp_reach->{$_} = 1 } $part->all_successors( $root );
                    # Skip this root if none of our successors are in our group
                    # (e.g. isolated 'hypothetical' witnesses with no group)
                    next unless grep { $contig->{$_} } keys %$tmp_reach;
                    if( keys %$tmp_reach > $nodes_in_subtree ) {
                        $nodes_in_subtree = keys %$tmp_reach;
                        $reachable = $tmp_reach;
                        $group_root = $root;
                    }
                }
            } # else it is a single-node group, nothing to calculate.
        }
        
        # None of the 'reachable' nodes should be marked as being in another 
        # group.  Paint the 'hypotheticals' with our group while we are at it,
        # unless there is a conflict present.
        foreach ( keys %$reachable ) {
            if( ref $contig->{$_} ) {
                push( @{$contig->{$_}}, $gst );
            } elsif( $contig->{$_} ne $gst ) {
                $conflict->{$group_readings->{$gst}} = $group_readings->{$contig->{$_}};
            } # else it is an 'extant' node marked with our group already.
        }
        # None of the unreachable nodes should be in our group either.
        foreach ( $part->vertices ) {
            next if $reachable->{$_};
            if( $contig->{$_} eq $gst ) {
                $conflict->{$group_readings->{$gst}} = $group_readings->{$gst};
                last;
            }
        }
        
        # Now, if we have a conflict, we can write the reading in full.  If not, 
        # we have to save the subgraph so that we can resolve possible conflicts 
        # on hypothetical nodes.
        $is_conflicted = 1 if exists $conflict->{$group_readings->{$gst}};
        
        # Write the reading.
        my $reading = { 'text' => $group_readings->{$gst},
                        'missing' => wit_stringify( $lacunose ),
                        'group' => $gst };  # This will change if we find no conflict
        if( $is_conflicted ) {
            $reading->{'conflict'} = $conflict->{$group_readings->{$gst}}
        } else {
            # Save the relevant subgraph.
            $subgraph->{$gst} = { 'graph' => $part,
                                'root' => $group_root,
                                'reachable' => $reachable };
        }
        push( @{$variant_row->{'readings'}}, $reading );
    }
    
    # Now that we have gone through all the rows, check the hypothetical
    # readings for conflict if we haven't found one yet.
    if( keys %$subgraph && !keys %$conflict ) {
        my @resolve;
        foreach ( keys %$contig ) {
            next unless ref $contig->{$_};
            if( scalar @{$contig->{$_}} > 1 ) {
                push( @resolve, $_ );
            } else {
                $contig->{$_} = scalar @{$contig->{$_}} ? $contig->{$_}->[0] : '';
            }
        }
        # Do we still have a possible conflict?
        my $still_contig = {};
        foreach my $h ( @resolve ) {
            # For each of the hypothetical readings with more than one possibility,
            # try deleting it from each of its member subgraphs in turn, and see
            # if that breaks the contiguous grouping.
            # TODO This can still break in a corner case where group A can use 
            # either vertex 1 or 2, and group B can use either vertex 2 or 1.
            # Revisit this if necessary; it could get brute-force nasty.
            foreach my $gst ( @{$contig->{$h}} ) {
                my $gpart = $subgraph->{$gst}->{'graph'}->copy;
                my $reachable = $subgraph->{$gst}->{'reachable'};
                $gpart->delete_vertex( $h );
                # Is everything else still reachable from the root?
                # TODO If $h was the root, see if we still have a single root.
                my %still_reachable = ( $subgraph->{$gst}->{'root'} => 1 );
                map { $still_reachable{$_} = 1 }
                    $gpart->all_successors( $subgraph->{$gst}->{'root'} );
                foreach my $v ( keys %$reachable ) {
                    next if $v eq $h;
                    if( !$still_reachable{$v}
                        && ( $contig->{$v} eq $gst 
                             || ( exists $still_contig->{$v} 
                                  && $still_contig->{$v} eq $gst ) ) ) {
                        # We need $h.
                        if( exists $still_contig->{$h} ) {
                            # Conflict!
                            $conflict->{$group_readings->{$gst}} = 
                                $group_readings->{$still_contig->{$h}};
                        } else {
                            $still_contig->{$h} = $gst;
                        }
                        last;
                    } # else we don't need $h in this group.
                }
            }
        }
        
        # Now, assuming no conflict, we have some hypothetical vertices in
        # $still_contig that are the "real" group memberships.  Replace these
        # in $contig.
        unless ( keys %$conflict ) {
            foreach my $v ( keys %$contig ) {
                next unless ref $contig->{$v};
                $contig->{$v} = $still_contig->{$v};
            }
        }
    }
            
    # Now write the group and conflict information into the respective rows.
    foreach my $rdg ( @{$variant_row->{'readings'}} ) {
        $rdg->{'conflict'} = $conflict->{$rdg->{'text'}};
        next if $rdg->{'conflict'};
        my @members = grep { $contig->{$_} eq $rdg->{'group'} && !$missing->{$_} } 
            keys %$contig;
        $rdg->{'group'} = wit_stringify( \@members );
    }
    
    $variant_row->{'genealogical'} = !( keys %$conflict );
    return $variant_row;
}

sub prune_subtree {
    my( $tree, $root, $contighash ) = @_;
    # First, delete hypothetical leaves / orphans until there are none left.
    my @orphan_hypotheticals = grep { ref( $contighash->{$_} ) } 
        $tree->successorless_vertices;
    while( @orphan_hypotheticals ) {
        $tree->delete_vertices( @orphan_hypotheticals );
        @orphan_hypotheticals = grep { ref( $contighash->{$_} ) } 
            $tree->successorless_vertices;
    }
    # Then delete a hypothetical root with only one successor, moving the
    # root to the child.
    while( $tree->successors( $root ) == 1 && ref $contighash->{$root} ) {
        my @nextroot = $tree->successors( $root );
        $tree->delete_vertex( $root );
        $root = $nextroot[0];
    }
    # The tree has been modified in place, but we need to know the new root.
    return $root;
}
# Add the variant, subject to a.c. representation logic.
# This assumes that we will see the 'main' version before the a.c. version.
sub add_variant_wit {
    my( $arr, $wit, $acstr ) = @_;
    my $skip;
    if( $wit =~ /^(.*)\Q$acstr\E$/ ) {
        my $real = $1;
        $skip = grep { $_ =~ /^\Q$real\E$/ } @$arr;
    } 
    push( @$arr, $wit ) unless $skip;
}

# Return an answer if the variant is useful, i.e. if there are at least 2 variants
# with at least 2 witnesses each.
sub useful_variant {
    my( $readings ) = @_;
    my $total = keys %$readings;
    foreach my $var ( keys %$readings ) {
        $total-- if @{$readings->{$var}} == 1;
    }
    return( undef, undef ) if $total <= 1;
    my( $groups, $text );
    foreach my $var ( keys %$readings ) {
        push( @$groups, $readings->{$var} );
        push( @$text, $var );
    }
    return( $groups, $text );
}

# Take an array of witness groupings and produce a string like
# ['A','B'] / ['C','D','E'] / ['F']

sub wit_stringify {
    my $groups = shift;
    my @gst;
    # If we were passed an array of witnesses instead of an array of 
    # groupings, then "group" the witnesses first.
    unless( ref( $groups->[0] ) ) {
        my $mkgrp = [ $groups ];
        $groups = $mkgrp;
    }
    foreach my $g ( @$groups ) {
        push( @gst, '[' . join( ',', map { "'$_'" } @$g ) . ']' );
    }
    return join( ' / ', @gst );
}
    
1;