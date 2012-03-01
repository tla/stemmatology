package Text::Tradition::Analysis;

use strict;
use warnings;
use Benchmark;
use Exporter 'import';
use Text::Tradition;
use Text::Tradition::Stemma;

use vars qw/ @EXPORT_OK /;
@EXPORT_OK = qw/ run_analysis group_variants analyze_variant_location wit_stringify /;

=head1 NAME

Text::Tradition::Analysis - functions for stemma analysis of a tradition

=head1 SYNOPSIS

  use Text::Tradition;
  use Text::Tradition::Analysis qw/ run_analysis analyze_variant_location /;
  my $t = Text::Tradition->new( 
    'name' => 'this is a text',
    'input' => 'TEI',
    'file' => '/path/to/tei_parallel_seg_file.xml' );
  $t->add_stemma( 'dotfile' => $stemmafile );

  my $variant_data = run_analysis( $tradition );
  # Recalculate rank $n treating all orthographic variants as equivalent
  my $reanalyze = analyze_variant_location( $tradition, $n, 0, 'orthographic' );
    
=head1 DESCRIPTION

Text::Tradition is a library for representation and analysis of collated
texts, particularly medieval ones.  The Collation is the central feature of
a Tradition, where the text, its sequence of readings, and its relationships
between readings are actually kept.

=head1 SUBROUTINES

=head2 run_analysis( $tradition, $stemma_id, @merge_relationship_types )

Runs the analysis described in analyze_variant_location on every location
in the collation of the given tradition, against the stemma specified in
$stemma_id.  If $stemma_id is not specified, it defaults to 0 (referencing
the first stemma saved for the tradition.)

The optional @merge_relationship_types contains a list of relationship types 
to treat as equivalent for the analysis.

=begin testing

use Text::Tradition;
use Text::Tradition::Analysis qw/ run_analysis analyze_variant_location /;

my $datafile = 't/data/florilegium_tei_ps.xml';
my $tradition = Text::Tradition->new( 'input' => 'TEI',
                                      'name' => 'test0',
                                      'file' => $datafile );
my $s = $tradition->add_stemma( 'dotfile' => 't/data/florilegium.dot' );
is( ref( $s ), 'Text::Tradition::Stemma', "Added stemma to tradition" );

my $data = run_analysis( $tradition );
# TODO Check genealogical count
is( $data->{'genealogical_count'}, 13, "Got right genealogical count" );
is( $data->{'conflict_count'}, 16, "Got right conflict count" );
is( $data->{'variant_count'}, 28, "Got right total variant number" );

=end testing

=cut

sub run_analysis {
	my( $tradition, $stemma_id, @collapse ) = @_;
	$stemma_id = 0 unless $stemma_id;
	
	# Run the variant analysis on every rank in the graph that doesn't
	# have a common reading. Return the results.
	my @variants; # holds results from analyze_variant_location
	my $genealogical; # counter of 'genealogical' variants
	my $conflicts;    # counter of conflicting readings
	
	# Find and mark 'common' ranks for exclusion.
	my %common_rank;
	foreach my $rdg ( $tradition->collation->common_readings ) {
		$common_rank{$rdg->rank} = 1;
	}
	
	foreach my $rank ( 1 .. $tradition->collation->end->rank-1 ) {
		next if $common_rank{$rank};
		my $variant_row = analyze_variant_location( 
			$tradition, $rank, $stemma_id, @collapse );
		next unless $variant_row;
		push( @variants, $variant_row );
		$genealogical++ if $variant_row->{'genealogical'};
		$conflicts += grep { $_->{'conflict'} } @{$variant_row->{'readings'}};
	}
	
	return {
		'variants' => \@variants,
		'variant_count' => scalar @variants, # TODO redundant
		'conflict_count' => $conflicts,
		'genealogical_count' => $genealogical,
		};
}

=head2 group_variants( $tradition, $rank, $lacunose, @merge_relationship_types )

Groups the variants at the given $rank of the collation, treating any
relationships in @merge_relationship_types as equivalent.  $lacunose should
be a reference to an array, to which the sigla of lacunose witnesses at this 
rank will be appended.

Returns two ordered lists $readings, $groups, where $readings->[$n] is attested
by the witnesses listed in $groups->[$n].

=cut

# Return group_readings, groups, lacunose
sub group_variants {
	my( $tradition, $rank, $lacunose, $collapse ) = @_;
	my $c = $tradition->collation;
	# Get the alignment table readings
	my %readings_at_rank;
	my @gap_wits;
	foreach my $tablewit ( @{$tradition->collation->alignment_table->{'alignment'}} ) {
		my $rdg = $tablewit->{'tokens'}->[$rank-1];
		if( $rdg && $rdg->{'t'}->is_lacuna ) {
			_add_to_witlist( $tablewit->{'witness'}, $lacunose, 
				$tradition->collation->ac_label );
		} elsif( $rdg ) {
			$readings_at_rank{$rdg->{'t'}->text} = $rdg->{'t'};
		} else {
			_add_to_witlist( $tablewit->{'witness'}, \@gap_wits, 
				$tradition->collation->ac_label );
		}
	}
	
	# Group the readings, collapsing groups by relationship if needed
	my %grouped_readings;
	foreach my $rdg ( sort { $b->witnesses <=> $a->witnesses } values %readings_at_rank ) {
		# Skip readings that have been collapsed into others.
		next if exists $grouped_readings{$rdg->text} && !$grouped_readings{$rdg->text};
		my @wits = $rdg->witnesses;
		if( $collapse ) {
			my $filter = sub { my $r = $_[0]; grep { $_ eq $r->type } @$collapse; };
			foreach my $other ( $rdg->related_readings( $filter ) ) {
				push( @wits, $other->witnesses );
				$grouped_readings{$other->text} = 0;
			}
		}
		$grouped_readings{$rdg->text} = \@wits;	
	}
	$grouped_readings{'(omitted)'} = \@gap_wits if @gap_wits;
	# Get rid of our collapsed readings
	map { delete $grouped_readings{$_} unless $grouped_readings{$_} } 
		keys %grouped_readings 
		if $collapse;
	
	return \%grouped_readings;
}

=head2 analyze_variant_location( $tradition, $rank, $stemma_id, @merge_relationship_types )

Runs an analysis of the given tradition, at the location given in $rank, 
against the graph of the stemma specified in $stemma_id.  The argument 
@merge_relationship_types is an optional list of relationship types for
which readings so related should be treated as equivalent.

Returns a data structure as follows:

 { 	'id' => $rank,
 	'genealogical' => boolean,
 	'readings => [ { text => $reading_text, 
 					 group => [ witnesses ], 
 					 conflict => [ conflicting ], 
 					 missing => [ excluded ] }, ... ]
 }
where 'conflicting' is the list of witnesses whose readings conflict with
this group, and 'excluded' is the list of witnesses either not present in
the stemma or lacunose at this location.

=cut

sub analyze_variant_location {
	my( $tradition, $rank, $sid, @collapse ) = @_;
	# Get the readings in this tradition at this rank
	my @rank_rdgs = grep { $_->rank == $rank } $tradition->collation->readings;
	# Get the applicable stemma
	my $undirected; # TODO Allow undirected distance tree analysis too
	my $stemma = $tradition->stemma( $sid );
	my $graph = $stemma->graph;
	# Figure out which witnesses we are working with
	my @lacunose = $stemma->hypotheticals;
	push( @lacunose, _symmdiff( [ $stemma->witnesses ], 
		[ map { $_->sigil } $tradition->witnesses ] ) );

	# Now group the readings
	my( $readings, $groups ) = _useful_variant( 
		group_variants( $tradition, $rank, \@lacunose, \@collapse ), 
		$graph, $tradition->collation->ac_label );
	return unless scalar @$readings;
	my $group_readings = {};
	# Lookup table group string -> readings
	foreach my $x ( 0 .. $#$groups ) {
		$group_readings->{wit_stringify( $groups->[$x] )} = $readings->[$x];
	}

	# Now do the work.	
    my $contig = {};
    my $subgraph = {};
    my $is_conflicted;
    my $conflict = {};
    my %reading_roots;
    my $variant_row = { 'id' => $rank, 'readings' => [] };
    # Mark each ms as in its own group, first.
    foreach my $g ( @$groups ) {
        my $gst = wit_stringify( $g );
        map { $contig->{$_} = $gst } @$g;
    }
    # Now for each unmarked node in the graph, initialize an array
    # for possible group memberships.  We will use this later to
    # resolve potential conflicts.
    $DB::single = 1 if $rank == 636;
    map { $contig->{$_} = [] unless $contig->{$_} } $graph->vertices;
    foreach my $g ( sort { scalar @$b <=> scalar @$a } @$groups ) {
        my $gst = wit_stringify( $g );  # This is the group name
        # Copy the graph, and delete all non-members from the new graph.
        my $part = $graph->copy;
        my @group_roots;
        $part->delete_vertices( 
            grep { !ref( $contig->{$_} ) && $contig->{$_} ne $gst } $graph->vertices );
                
        # Now look to see if our group is connected.
        if( $undirected ) { # For use with distance trees etc.
            # Find all vertices reachable from the first (arbitrary) group
            # member.  If we are genealogical this should include them all.
            my $reachable = {}; 
            map { $reachable->{$_} = 1 } $part->all_reachable( $g->[0] );
            # TODO This is a terrible way to do distance trees, since all
            # non-leaf nodes are included in every graph part now. We may
            # have to go back to SPDP.
        } else {
            if( @$g > 1 ) {
                # We have to take directionality into account.
                # How many root nodes do we have?
                my @roots = grep { ref( $contig->{$_} ) || $contig->{$_} eq $gst } 
                    $part->predecessorless_vertices;
                # Assuming that @$g > 1, find the first root node that has at
                # least one successor belonging to our group. If this reading
                # is genealogical, there should be only one, but we will check
                # that implicitly later.
                foreach my $root ( @roots ) {
                    # Prune the tree to get rid of extraneous hypotheticals.
                    $root = _prune_subtree( $part, $root, $contig );
                    next unless $root;
                    # Save this root for our group.
                    push( @group_roots, $root );
                    # Get all the successor nodes of our root.
                }
            } else {
            	# Dispense with the trivial case of one reading.
                @group_roots = @$g;
                _prune_subtree( $part, @group_roots, $contig );
            }
        }
        
        map { $reading_roots{$_} = 1 } @group_roots;
        if( @group_roots > 1 ) {
        	$conflict->{$group_readings->{$gst}} = 1;
        	$is_conflicted = 1;
        }
        # Paint the 'hypotheticals' with our group.
		foreach my $wit ( $part->vertices ) {
			if( ref( $contig->{$wit} ) ) {
				push( @{$contig->{$wit}}, $gst );
			} elsif( $contig->{$wit} ne $gst ) {
				warn "How did we get here?";
			}
		}
        
        
        # Start to write the reading, and save the group subgraph.
        my $reading = { 'text' => $group_readings->{$gst},
                        'missing' => wit_stringify( \@lacunose ),
                        'group' => $gst };  # This will change if we find no conflict
		# Save the relevant subgraph.
		$subgraph->{$gst} = $part;
        push( @{$variant_row->{'readings'}}, $reading );
    }
    
	# For each of our hypothetical readings, flatten its 'contig' array if
	# the array contains zero or one group.  If we have any unflattened arrays,
	# we may need to run the resolution process. If the reading is already known
	# to have a conflict, flatten the 'contig' array to nothing; we won't resolve
	# it.
	my @resolve;
	foreach my $wit ( keys %$contig ) {
		next unless ref( $contig->{$wit} );
		if( @{$contig->{$wit}} > 1 ) {
			if( $is_conflicted ) {
				$contig->{$wit} = '';  # We aren't going to decide.
			} else {
				push( @resolve, $wit );			
			}
		} else {
			my $gst = pop @{$contig->{$wit}};
			$contig->{$wit} = $gst || '';
		}
	}
	
    if( @resolve ) {
        my $still_contig = {};
        foreach my $h ( @resolve ) {
            # For each of the hypothetical readings with more than one possibility,
            # try deleting it from each of its member subgraphs in turn, and see
            # if that breaks the contiguous grouping.
            # TODO This can still break in a corner case where group A can use 
            # either vertex 1 or 2, and group B can use either vertex 2 or 1.
            # Revisit this if necessary; it could get brute-force nasty.
            foreach my $gst ( @{$contig->{$h}} ) {
                my $gpart = $subgraph->{$gst}->copy();
                # If we have come this far, there is only one root and everything
                # is reachable from it.
                my( $root ) = $gpart->predecessorless_vertices;    
                my $reachable = {};
                map { $reachable->{$_} = 1 } $gpart->vertices;

                # Try deleting the hypothetical node. 
                $gpart->delete_vertex( $h );
                if( $h eq $root ) {
                	# See if we still have a single root.
                	my @roots = $gpart->predecessorless_vertices;
                	warn "This shouldn't have happened" unless @roots;
                	if( @roots > 1 ) {
                		# $h is needed by this group.
                		if( exists( $still_contig->{$h} ) ) {
                			# Conflict!
                			$conflict->{$group_readings->{$gst}} = 1;
                			$still_contig->{$h} = '';
                		} else {
                			$still_contig->{$h} = $gst;
                		}
                	}
                } else {
                	# $h is somewhere in the middle. See if everything
                	# else can still be reached from the root.
					my %still_reachable = ( $root => 1 );
					map { $still_reachable{$_} = 1 }
						$gpart->all_successors( $root );
					foreach my $v ( keys %$reachable ) {
						next if $v eq $h;
						if( !$still_reachable{$v}
							&& ( $contig->{$v} eq $gst 
								 || ( exists $still_contig->{$v} 
									  && $still_contig->{$v} eq $gst ) ) ) {
							# We need $h.
							if( exists $still_contig->{$h} ) {
								# Conflict!
								$conflict->{$group_readings->{$gst}} = 1;
								$still_contig->{$h} = '';
							} else {
								$still_contig->{$h} = $gst;
							}
							last;
						} # else we don't need $h in this group.
					} # end foreach $v
				} # endif $h eq $root
            } # end foreach $gst
        } # end foreach $h
        
        # Now we have some hypothetical vertices in $still_contig that are the 
        # "real" group memberships.  Replace these in $contig.
		foreach my $v ( keys %$contig ) {
			next unless ref $contig->{$v};
			$contig->{$v} = $still_contig->{$v};
		}
    } # end if @resolve
    
    # Now that we have all the node group memberships, calculate followed/
    # non-followed/unknown values for each reading.  Also figure out the
    # reading's evident parent(s).
    foreach my $rdghash ( @{$variant_row->{'readings'}} ) {
        my $gst = $rdghash->{'group'};
        my $part = $subgraph->{$gst};
        my @roots = $part->predecessorless_vertices;
        $rdghash->{'independent_occurrence'} = scalar @roots;
        $rdghash->{'followed'} = scalar( $part->vertices ) - scalar( @roots );
        # Find the parent readings, if any, of this reading.
        my @rdgparents;
        foreach my $wit ( @roots ) {
        	# Look in the main stemma to find this witness's parent(s), and look
        	# up the reading that the parent holds.
        	foreach my $wparent( $graph->predecessors( $wit ) ) {
        		my $pgroup = $contig->{$wparent};
        		if( $pgroup ) {
        			push( @rdgparents, $group_readings->{$pgroup} );
        		}
        	}
		}
		$rdghash->{'reading_parents'} = \@rdgparents;
		
		# Find the number of times this reading was altered, and the number of
		# times we're not sure.
		my( %nofollow, %unknownfollow );
		foreach my $wit ( $part->vertices ) {
			foreach my $wchild ( $graph->successors( $wit ) ) {
				next if $part->has_vertex( $wchild );
				if( $reading_roots{$wchild} && $contig->{$wchild} ) {
					# It definitely changed here.
					$nofollow{$wchild} = 1;
				} elsif( !($contig->{$wchild}) ) {
					# The child is a hypothetical node not definitely in
					# any group. Answer is unknown.
					$unknownfollow{$wchild} = 1;
				} # else it's a non-root node in a known group, and therefore
				  # is presumed to have its reading from its group, not this link.
			}
		}
		$rdghash->{'not_followed'} = keys %nofollow;
		$rdghash->{'follow_unknown'} = keys %unknownfollow;
    }
    
    # Now write the group and conflict information into the respective rows.
    foreach my $rdghash ( @{$variant_row->{'readings'}} ) {
        $rdghash->{'conflict'} = $conflict->{$rdghash->{'text'}};
        my @members = grep { $contig->{$_} eq $rdghash->{'group'} } keys %$contig;
        $rdghash->{'group'} = wit_stringify( \@members );
    }
    
    $variant_row->{'genealogical'} = !( keys %$conflict );
    return $variant_row;
}

sub _prune_subtree {
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
    # root to the first child that has no other predecessors.
    while( $tree->successors( $root ) == 1 && ref $contighash->{$root} ) {
        my @nextroot = $tree->successors( $root );
        $tree->delete_vertex( $root );
        ( $root ) = grep { $tree->is_predecessorless_vertex( $_ ) } @nextroot;
    }
    # The tree has been modified in place, but we need to know the new root.
    $root = undef unless $root && $tree->has_vertex( $root );
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

sub _useful_variant {
	my( $group_readings, $graph, $acstr ) = @_;

	# TODO Decide what to do with AC witnesses

	# Sort by group size and return
	my $is_useful = 0;
	my( @readings, @groups );   # The sorted groups for our answer.
	foreach my $rdg ( sort { @{$group_readings->{$b}} <=> @{$group_readings->{$a}} } 
		keys %$group_readings ) {
		push( @readings, $rdg );
		push( @groups, $group_readings->{$rdg} );
		if( @{$group_readings->{$rdg}} > 1 ) {
			$is_useful++;
		} else {
			my( $wit ) = @{$group_readings->{$rdg}};
			$wit =~ s/^(.*)\Q$acstr\E$/$1/;
			$is_useful++ unless( $graph->is_sink_vertex( $wit ) );
		}
	}
	if( $is_useful > 1 ) {
		return( \@readings, \@groups );
	} else {
		return( [], [] );
	}
}

=head2 wit_stringify( $groups )

Takes an array of witness groupings and produces a string like
['A','B'] / ['C','D','E'] / ['F']

=cut

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

# Helper function to ensure that X and X a.c. never appear in the same list.
sub _add_to_witlist {
	my( $wit, $list, $acstr ) = @_;
	my %inlist;
	my $idx = 0;
	map { $inlist{$_} = $idx++ } @$list;
	if( $wit =~ /^(.*)\Q$acstr\E$/ ) {
		my $acwit = $1;
		unless( exists $inlist{$acwit} ) {
			push( @$list, $acwit.$acstr );
		}
	} else {
		if( exists( $inlist{$wit.$acstr} ) ) {
			# Replace the a.c. version with the main witness
			my $i = $inlist{$wit.$acstr};
			$list->[$i] = $wit;
		} else {
			push( @$list, $wit );
		}
	}
}

sub _symmdiff {
	my( $lista, $listb ) = @_;
	my %union;
	my %scalars;
	map { $union{$_} = 1; $scalars{$_} = $_ } @$lista;
	map { $union{$_} += 1; $scalars{$_} = $_ } @$listb;
	my @set = grep { $union{$_} == 1 } keys %union;
	return map { $scalars{$_} } @set;
}

1;

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews E<lt>aurum@cpan.orgE<gt>
