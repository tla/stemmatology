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
	my @lacunose = _set( 'symmdiff', [ $stemma->witnesses ], 
		[ map { $_->sigil } $tradition->witnesses ] );

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
    my $variant_row = { 'id' => $rank, 'readings' => [] };
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
                    $root = _prune_subtree( $part, $root, $contig );
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
                        'missing' => wit_stringify( \@lacunose ),
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
    my %missing;
	map { $missing{$_} = 1 } @lacunose; # quick lookup table
    foreach my $rdg ( @{$variant_row->{'readings'}} ) {
        $rdg->{'conflict'} = $conflict->{$rdg->{'text'}};
        next if $rdg->{'conflict'};
        my @members = grep { $contig->{$_} eq $rdg->{'group'} && !$missing{$_} } 
            keys %$contig;
        $rdg->{'group'} = wit_stringify( \@members );
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

sub _set {
	my( $op, $lista, $listb ) = @_;
	my %union;
	my %scalars;
	map { $union{$_} = 1; $scalars{$_} = $_ } @$lista;
	map { $union{$_} += 1; $scalars{$_} = $_ } @$listb;
	my @set;
	if( $op eq 'intersection' ) {
		@set = grep { $union{$_} == 2 } keys %union;
	} elsif( $op eq 'symmdiff' ) {
		@set = grep { $union{$_} == 1 } keys %union;
	} elsif( $op eq 'union' ) {
		@set = keys %union;
	}
	return map { $scalars{$_} } @set;
}

1;

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews E<lt>aurum@cpan.orgE<gt>
