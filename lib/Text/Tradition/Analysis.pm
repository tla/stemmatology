package Text::Tradition::Analysis;

use strict;
use warnings;
use Algorithm::Diff;  # for word similarity measure
use Benchmark;
use Encode qw/ encode_utf8 /;
use Exporter 'import';
use Graph;
use JSON qw/ encode_json decode_json /;
use LWP::UserAgent;
use Text::Tradition;
use Text::Tradition::Stemma;
use TryCatch;

use vars qw/ @EXPORT_OK /;
@EXPORT_OK = qw/ run_analysis group_variants analyze_variant_location wit_stringify /;

my $SOLVER_URL = 'http://byzantini.st/cgi-bin/graphcalc.cgi';
	

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

=head2 run_analysis( $tradition, %opts )

Runs the analysis described in analyze_variant_location on every location in the 
collation of the given tradition, with the given options. These include:

=over 4

=item * stemma_id - Specify which of the tradition's stemmata to use. Default
is 0 (i.e. the first).

=item * ranks - Specify a list of location ranks to analyze; exclude the rest.

=item * merge_types - Specify a list of relationship types, where related readings 
should be treated as identical for the purposes of analysis.

=item * exclude_type1 - Exclude those ranks whose groupings have only type-1 variants.

=back

=begin testing

use Text::Tradition;
use Text::Tradition::Analysis qw/ run_analysis analyze_variant_location /;

my $datafile = 't/data/florilegium_tei_ps.xml';
my $tradition = Text::Tradition->new( 'input' => 'TEI',
                                      'name' => 'test0',
                                      'file' => $datafile );
my $s = $tradition->add_stemma( 'dotfile' => 't/data/florilegium.dot' );
is( ref( $s ), 'Text::Tradition::Stemma', "Added stemma to tradition" );

my %expected_genealogical = (
	1 => 0,
	2 => 1,
	3 =>  0,
	5 =>  0,
	7 =>  0,
	8 =>  0,
	10 => 0,
	13 => 1,
	33 => 0,
	34 => 0,
	37 => 0,
	60 => 0,
	81 => 1,
	84 => 0,
	87 => 0,
	101 => 0,
	102 => 0,
	122 => 1,
	157 => 0,
	166 => 1,
	169 => 1,
	200 => 0,
	216 => 1,
	217 => 1,
	219 => 1,
	241 => 1,
	242 => 1,
	243 => 1,
);

my $data = run_analysis( $tradition );
my $c = $tradition->collation;
foreach my $row ( @{$data->{'variants'}} ) {
	# Account for rows that used to be "not useful"
	unless( exists $expected_genealogical{$row->{'id'}} ) {
		$expected_genealogical{$row->{'id'}} = 1;
	}
	my $gen_bool = $row->{'genealogical'} ? 1 : 0;
	is( $gen_bool, $expected_genealogical{$row->{'id'}}, 
		"Got correct genealogical flag for row " . $row->{'id'} );
	# Check that we have the right row with the right groups
	my $rank = $row->{'id'};
	foreach my $rdghash ( @{$row->{'readings'}} ) {
		# Skip 'readings' that aren't really
		next unless $c->reading( $rdghash->{'readingid'} );
		# Check the rank
		is( $c->reading( $rdghash->{'readingid'} )->rank, $rank, 
			"Got correct reading rank" );
		# Check the witnesses
		my @realwits = sort $c->reading_witnesses( $rdghash->{'readingid'} );
		my @sgrp = sort @{$rdghash->{'group'}};
		is_deeply( \@sgrp, \@realwits, "Reading analyzed with correct groups" );
	}
}
is( $data->{'variant_count'}, 58, "Got right total variant number" );
# TODO Make something meaningful of conflict count, maybe test other bits

=end testing

=cut

sub run_analysis {
	my( $tradition, %opts ) = @_;
	my $c = $tradition->collation;

	my $stemma_id = $opts{'stemma_id'} || 0;
	my @ranks = ref( $opts{'ranks'} ) eq 'ARRAY' ? @{$opts{'ranks'}} : ();
	my @collapse = ref( $opts{'merge_types'} ) eq 'ARRAY' ? @{$opts{'merge_types'}} : ();

	# Get the stemma	
	my $stemma = $tradition->stemma( $stemma_id );

	# Figure out which witnesses we are working with - that is, the ones that
	# appear both in the stemma and in the tradition. All others are 'lacunose'
	# for our purposes.
	my @lacunose = $stemma->hypotheticals;
	my @tradition_wits = map { $_->sigil } $tradition->witnesses;
	push( @lacunose, _symmdiff( [ $stemma->witnesses ], \@tradition_wits ) );

	# Find and mark 'common' ranks for exclusion, unless they were
	# explicitly specified.
	unless( @ranks ) {
		my %common_rank;
		foreach my $rdg ( $c->common_readings ) {
			$common_rank{$rdg->rank} = 1;
		}
		@ranks = grep { !$common_rank{$_} } ( 1 .. $c->end->rank-1 );
	}
	
	# Group the variants to send to the solver
	my @groups;
	my @use_ranks;
	my %lacunae;
	my $moved = {};
	foreach my $rank ( @ranks ) {
		my $missing = [ @lacunose ];
		my $rankgroup = group_variants( $tradition, $rank, $missing, $moved, \@collapse );
		# Filter out any empty rankgroups 
		# (e.g. from the later rank for a transposition)
		next unless keys %$rankgroup;
		if( $opts{'exclude_type1'} ) {
			# Check to see whether this is a "useful" group.
			my( $rdgs, $grps ) = _useful_variant( $rankgroup, 
				$stemma->graph, $c->ac_label );
			next unless @$rdgs;
		}
		push( @use_ranks, $rank );
		push( @groups, $rankgroup );
		$lacunae{$rank} = $missing;
	}
	# Run the solver
	my $answer = solve_variants( $stemma, @groups );

	# Do further analysis on the answer
	my $conflict_count = 0;
	my $aclabel = $c->ac_label;
	foreach my $idx ( 0 .. $#use_ranks ) {
		my $location = $answer->{'variants'}->[$idx];
		# Add the rank back in
		my $rank = $use_ranks[$idx];
		$location->{'id'} = $rank;
		# Note what our lacunae are
		my %lmiss;
		map { $lmiss{$_} = 1 } @{$lacunae{$use_ranks[$idx]}};
		$location->{'missing'} = [ keys %lmiss ];
		
		# Run the extra analysis we need.
		## TODO We run through all the variants in this call, so
		## why not add the reading data there instead of here below?
		analyze_location( $tradition, $stemma, $location, \%lmiss );

		my @layerwits;
		# Do the final post-analysis tidying up of the data.
		foreach my $rdghash ( @{$location->{'readings'}} ) {
			$conflict_count++ 
				if exists $rdghash->{'conflict'} && $rdghash->{'conflict'};
			# Add the reading text back in, setting display value as needed
			my $rdg = $c->reading( $rdghash->{'readingid'} );
			if( $rdg ) {
				$rdghash->{'text'} = $rdg->text . 
					( $rdg->rank == $rank ? '' : ' [' . $rdg->rank . ']' );
				$rdghash->{'is_ungrammatical'} = $rdg->grammar_invalid;
				$rdghash->{'is_nonsense'} = $rdg->is_nonsense;
			}
			# Remove lacunose witnesses from this reading's list now that the
			# analysis is done 
			my @realgroup;
			map { push( @realgroup, $_ ) unless $lmiss{$_} } @{$rdghash->{'group'}};
			$rdghash->{'group'} = \@realgroup;
			# Note any layered witnesses that appear in this group
			foreach( @realgroup ) {
				if( $_ =~ /^(.*)\Q$aclabel\E$/ ) {
					push( @layerwits, $1 );
				}
			}
		}
		$location->{'layerwits'} = \@layerwits if @layerwits;
	}
	$answer->{'conflict_count'} = $conflict_count;
	
	return $answer;
}

=head2 group_variants( $tradition, $rank, $lacunose, @merge_relationship_types )

Groups the variants at the given $rank of the collation, treating any
relationships in @merge_relationship_types as equivalent.  $lacunose should
be a reference to an array, to which the sigla of lacunose witnesses at this 
rank will be appended; $transposed should be a reference to a hash, wherein
the identities of transposed readings and their relatives will be stored.

Returns a hash $group_readings where $rdg is attested by the witnesses listed 
in $group_readings->{$rdg}.

=cut

# Return group_readings, groups, lacunose
sub group_variants {
	my( $tradition, $rank, $lacunose, $transposed, $collapse ) = @_;
	my $c = $tradition->collation;
	my $aclabel = $c->ac_label;
	my $table = $c->alignment_table;
	# Get the alignment table readings
	my %readings_at_rank;
	my %is_lacunose; # lookup table for witnesses not in stemma
	map { $is_lacunose{$_} = 1; $is_lacunose{$_.$aclabel} = 1 } @$lacunose;
	my @check_for_gaps;
	my %moved_wits;
	my $has_transposition;
	foreach my $tablewit ( @{$table->{'alignment'}} ) {
		my $rdg = $tablewit->{'tokens'}->[$rank-1];
		my $wit = $tablewit->{'witness'};
		# Exclude the witness if it is "lacunose" which if we got here
		# means "not in the stemma".
		next if $is_lacunose{$wit};
		# Note if the witness is actually in a lacuna
		if( $rdg && $rdg->{'t'}->is_lacuna ) {
			_add_to_witlist( $wit, $lacunose, $aclabel );
		# Otherwise the witness either has a positive reading...
		} elsif( $rdg ) {
			# If the reading has been counted elsewhere as a transposition, ignore it.
			if( $transposed->{$rdg->{'t'}->id} ) {
				# TODO Does this cope with three-way transpositions?
				map { $moved_wits{$_} = 1 } @{$transposed->{$rdg->{'t'}->id}};
				next;
			}
			# Otherwise, record it...
			$readings_at_rank{$rdg->{'t'}->id} = $rdg->{'t'};
			# ...and grab any transpositions, and their relations.
			my @transp = grep { $_->rank != $rank } $rdg->{'t'}->related_readings();
			foreach my $trdg ( @transp ) {
				next if exists $readings_at_rank{$trdg->id};
				$has_transposition = 1;
				my @affected_wits = _table_witnesses( 
					$table, $trdg, \%is_lacunose, $aclabel );
				next unless @affected_wits;
				map { $moved_wits{$_} = 1 } @affected_wits;
				$transposed->{$trdg->id} = 
					[ _table_witnesses( $table, $rdg->{'t'}, \%is_lacunose, $aclabel ) ];
				$readings_at_rank{$trdg->id} = $trdg;
			}
		# ...or it is empty, ergo a gap.
		} else {
			_add_to_witlist( $wit, \@check_for_gaps, $aclabel );
		}
	}
	my @gap_wits;
	map { _add_to_witlist( $_, \@gap_wits, $aclabel ) 
		unless $moved_wits{$_} } @check_for_gaps;
	# Group the readings, collapsing groups by relationship if needed
	my $grouped_readings = {};
	foreach my $rdg ( values %readings_at_rank ) {
		# Skip readings that have been collapsed into others.
		next if exists $grouped_readings->{$rdg->id} 
			&& $grouped_readings->{$rdg->id} eq 'COLLAPSE';
		# Get the witness list, including from readings collapsed into this one.
		my @wits = _table_witnesses( $table, $rdg, \%is_lacunose, $aclabel );
		if( $collapse && @$collapse ) {
			my $filter = sub { my $r = $_[0]; grep { $_ eq $r->type } @$collapse; };
			foreach my $other ( $rdg->related_readings( $filter ) ) {
				my @otherwits = _table_witnesses( 
					$table, $other, \%is_lacunose, $aclabel );
				push( @wits, @otherwits );
				$grouped_readings->{$other->id} = 'COLLAPSE';
			}
		}
		$grouped_readings->{$rdg->id} = \@wits;
	}
	$grouped_readings->{'(omitted)'} = \@gap_wits if @gap_wits;
	# Get rid of our collapsed readings
	map { delete $grouped_readings->{$_} if $grouped_readings->{$_} eq 'COLLAPSE' } 
		keys %$grouped_readings 
		if $collapse;
		
	# If something was transposed, check the groups for doubled-up readings
	if( $has_transposition ) {
		# print STDERR "Group for rank $rank:\n";
		# map { print STDERR "\t$_: " . join( ' ' , @{$grouped_readings->{$_}} ) . "\n" } 
		# 	keys %$grouped_readings;
		_check_transposed_consistency( $c, $rank, $transposed, $grouped_readings );
	}
	
	# Return the result
	return $grouped_readings;
}

# Helper function to query the alignment table for all witnesses (a.c. included)
# that have a given reading at its rank.
sub _table_witnesses {
	my( $table, $trdg, $lacunose, $aclabel ) = @_;
	my $tableidx = $trdg->rank - 1;
	my @has_reading;
	foreach my $row ( @{$table->{'alignment'}} ) {
		my $wit = $row->{'witness'};
		next if $lacunose->{$wit};
		my $rdg = $row->{'tokens'}->[$tableidx];
		next unless exists $rdg->{'t'} && defined $rdg->{'t'};
		_add_to_witlist( $wit, \@has_reading, $aclabel )
			if $rdg->{'t'}->id eq $trdg->id;
	}
	return @has_reading;
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

sub _check_transposed_consistency {
	my( $c, $rank, $transposed, $groupings ) = @_;
	my %seen_wits;
	my %thisrank;
	# Note which readings are actually at this rank, and which witnesses
	# belong to which reading.
	foreach my $rdg ( keys %$groupings ) {
		my $rdgobj = $c->reading( $rdg );
		# Count '(omitted)' as a reading at this rank
		$thisrank{$rdg} = 1 if !$rdgobj || $rdgobj->rank == $rank;
		map { push( @{$seen_wits{$_}}, $rdg ) } @{$groupings->{$rdg}};
	}
	# Our work is done if we have no witness belonging to more than one
	# reading.
	my @doubled = grep { scalar @{$seen_wits{$_}} > 1 } keys %seen_wits;
	return unless @doubled;
	# If we have a symmetric related transposition, drop the non-rank readings.
	if( @doubled == scalar keys %seen_wits ) {
		foreach my $rdg ( keys %$groupings ) {
			if( !$thisrank{$rdg} ) {
				my $groupstr = wit_stringify( $groupings->{$rdg} );
				my ( $matched ) = grep { $groupstr eq wit_stringify( $groupings->{$_} ) }
					keys %thisrank;
				delete $groupings->{$rdg};
				# If we found a group match, assume there is a symmetry happening.
				# TODO think more about this
				# print STDERR "*** Deleting symmetric reading $rdg\n";
				unless( $matched ) {
					delete $transposed->{$rdg};
					warn "Found problem in evident symmetry with reading $rdg";
				}
			}
		}
	# Otherwise 'unhook' the transposed reading(s) that have duplicates.
	} else {
		foreach my $dup ( @doubled ) {
			foreach my $rdg ( @{$seen_wits{$dup}} ) {
				next if $thisrank{$rdg};
				next unless exists $groupings->{$rdg};
				# print STDERR "*** Deleting asymmetric doubled-up reading $rdg\n";
				delete $groupings->{$rdg};
				delete $transposed->{$rdg};
			}
		}
		# and put any now-orphaned readings into an 'omitted' reading.
		foreach my $wit ( keys %seen_wits ) {
			unless( grep { exists $groupings->{$_} } @{$seen_wits{$wit}} ) {
				$groupings->{'(omitted)'} = [] unless exists $groupings->{'(omitted)'};
				_add_to_witlist( $wit, $groupings->{'(omitted)'}, $c->ac_label );
			}
		}
	}
}

=head2 solve_variants( $graph, @groups ) 

Sends the set of groups to the external graph solver service and returns
a cleaned-up answer, adding the rank IDs back where they belong.

The JSON has the form 
  { "graph": [ stemmagraph DOT string without newlines ],
    "groupings": [ array of arrays of groups, one per rank ] }
    
The answer has the form 
  { "variants" => [ array of variant location structures ],
    "variant_count" => total,
    "conflict_count" => number of conflicts detected,
    "genealogical_count" => number of solutions found }
    
=cut

sub solve_variants {
	my( $stemma, @groups ) = @_;

	# Filter the groups down to distinct groups, and work out what graph
	# should be used in the calculation of each group. We want to send each
	# distinct problem to the solver only once.
	# We need a whole bunch of lookup tables for this.
	my( $index_groupkeys, $group_indices, $graph_problems ) = _prepare_groups( @_ );

	## For each distinct graph, send its groups to the solver.
	my $ua = LWP::UserAgent->new();
	## Witness map is a HACK to get around limitations in node names from IDP
	my $witness_map = {};
	## Variables to store answers as they come back
	my $variants = [ ( undef ) x ( scalar keys %$index_groupkeys ) ];
	my $genealogical = 0;
	foreach my $graphkey ( keys %$graph_problems ) {
		my $graph = $graph_problems->{$graphkey}->{'object'};
		my $groupings = [ values %{$graph_problems->{$graphkey}->{'groups'}} ];
		my $req = _safe_wit_strings( $graph, $stemma->collation,
			$groupings, $witness_map );
		$req->{'command'} = 'findGroupings';
		my $json = encode_json( $req );
		# Send it off and get the result
		# print STDERR "Sending request: " . to_json( $req ) . "\n";
		my $resp = $ua->post( $SOLVER_URL, 'Content-Type' => 'application/json', 
							  'Content' => $json );							  
		my $answer;
		if( $resp->is_success ) {
			$answer = _desanitize_names( decode_json( $resp->content ), $witness_map );
		} else {
			# Fall back to the old method.
			die "IDP solver returned " . $resp->status_line . " / " . $resp->content
				. "; cannot run graph analysis";
		}
		
		## If IDP worked, asked it the other two questions for this dataset.
		my $more_eval = {};
		foreach my $test ( qw/ findSources findClasses / ) {
			$req->{'command'} = $test;
			$json = encode_json( $req );
			$resp = $ua->post( $SOLVER_URL, 'Content-Type' => 'application/json', 
							   'Content' => $json );
			if( $resp->is_success ) {
				$more_eval->{$test} = _desanitize_names( 
					decode_json( $resp->content ), $witness_map );
			} else {
				warn "IDP solver for $test returned " . $resp->status_line . 
					" / " . $resp->content;
				# TODO arrange fallback
			}
		}
		
		## The answer is the evaluated groupings, plus a boolean for whether
		## they were genealogical.  Reconstruct our original groups.
		foreach my $gidx ( 0 .. $#{$groupings} ) {
			my( $calc_groups, $result ) = @{$answer->[$gidx]};
			# Keep track of the total # of genealogical readings
			$genealogical++ if $result;
			
			my( $sources, $classes );
			# Use the expanded groups from findSources if that got calculated.
			if( exists( $more_eval->{'findSources'} ) ) {
				( $calc_groups, $sources ) = @{$more_eval->{'findSources'}->[$gidx]};
			}
			# Use the (same) expanded groups from findClasses if that got calculated
			# and is relevant.
			if( exists( $more_eval->{'findClasses'} ) && !$result ) {
				( $calc_groups, $classes ) = @{$more_eval->{'findClasses'}->[$gidx]};
			}
			
			# Prune the calculated groups, in case the IDP solver failed to.
			if( $sources || $result ) {
				my @pruned_groups;
				my @pruned_roots;
				foreach my $cg ( @$calc_groups ) {
					my( $pg, $pr ) = _prune_group( $cg, $graph );
					push( @pruned_groups, $pg );
					push( @pruned_roots, @$pr );
				}
				$calc_groups = \@pruned_groups;
				say STDERR "Pruned roots from @$sources to @pruned_roots"
					unless wit_stringify( [ sort @$sources ] ) 
						eq wit_stringify( [ sort @pruned_roots ] );
				$sources = \@pruned_roots;
			}
			
			# Convert the source list into a lookup hash
			my $roots = {};
			map { $roots->{$_} = 1 } @$sources;
			# Convert the class list into a lookup hash
			if( $classes ) {
				$classes = _invert_hash( $classes );
			}
			
			# Retrieve the key for the original group that went to the solver
			my $input_group = wit_stringify( $groupings->[$gidx] );

			# Make the variant hash for each location that had this particular
			# grouping on this particular stemma situation
			foreach my $oidx ( @{$group_indices->{$input_group}} ) {
				my @readings = @{$index_groupkeys->{$oidx}};
				my $vstruct = {
					'genealogical' => $result,
					'readings' => [],
				};
				foreach my $ridx ( 0 .. $#readings ) {
					push( @{$vstruct->{'readings'}},
						{ 'readingid' => $readings[$ridx],
						  'group' => $calc_groups->[$ridx] } );
				}
				$vstruct->{'reading_roots'} = $roots if $roots;
				$vstruct->{'reading_types'} = $classes if $classes;
				$variants->[$oidx] = $vstruct;
			}
		}
	}
	
	return { 'variants' => $variants, 
			 'variant_count' => scalar @$variants,
			 'genealogical_count' => $genealogical };
}

sub _prepare_groups {
	my( $stemma, @groups ) = @_;
	my $aclabel = $stemma->collation->ac_label;

	my $index_groupkeys = {};	# Save the order of readings
	my $group_indices = {};		# Save the indices that have a given grouping
	my $graph_problems = {};	# Save the groupings for the given graph

	foreach my $idx ( 0..$#groups ) {
		my $ghash = $groups[$idx];
		my @grouping;
		# Sort the groupings from big to little, and scan for a.c. witnesses
		# that would need an extended graph.
		my @acwits;   # note which AC witnesses crop up at this rank
		my $extant;   # note which witnesses crop up at this rank full stop
		my @idxkeys = sort { scalar @{$ghash->{$b}} <=> scalar @{$ghash->{$a}} }
			keys %$ghash;
		foreach my $rdg ( @idxkeys ) {
			my @sg = sort @{$ghash->{$rdg}};
			push( @acwits, grep { $_ =~ /\Q$aclabel\E$/ } @sg );
			map { $extant->{$_} = 1 } @sg;
			push( @grouping, \@sg );
		}
		# Save the reading order
		$index_groupkeys->{$idx} = \@idxkeys;
		
		# Now associate the distinct group with this index
		my $gstr = wit_stringify( \@grouping );
		push( @{$group_indices->{$gstr}}, $idx );
		
		# Finally, add the group to the list to be calculated for this graph.
		map { s/\Q$aclabel\E$// } @acwits;
		my $graph;
		## TODO When we get rid of the safe_wit_strings HACK we should also
		## be able to save the graph here as a dotstring rather than as an
		## object, thus simplifying life enormously.
		try {
			$graph = $stemma->situation_graph( $extant, \@acwits );
		} catch {
			$DB::single = 1;
			die "Unable to extend graph with @acwits";
		}
		my $graphkey = "$graph || " . wit_stringify( [ sort keys %$extant ] );
		unless( exists $graph_problems->{$graphkey} ) {
			$graph_problems->{$graphkey} = { 'object' => $graph, 'groups' => {} };
		}
		$graph_problems->{$graphkey}->{'groups'}->{wit_stringify( \@grouping )} = \@grouping;
	}
	say STDERR "Created " . scalar( keys %$graph_problems ). " distinct graph(s)";
	return( $index_groupkeys, $group_indices, $graph_problems );	
}

#### HACKERY to cope with IDP's limited idea of what a node name looks like ###

sub _safe_wit_strings {
	my( $graph, $c, $groupings, $witness_map ) = @_;
	# Convert the graph to a safe representation and store the conversion.
	my $safegraph = Graph->new();
	foreach my $n ( $graph->vertices ) {
		my $sn = _safe_witstr( $n );
		if( exists $witness_map->{$sn} ) {
			warn "Ambiguous stringification $sn for $n and " . $witness_map->{$sn}
				if $witness_map->{$sn} ne $n;
		} else {
			$witness_map->{$sn} = $n;
		}
		$safegraph->add_vertex( $sn );
		$safegraph->set_vertex_attributes( $sn, 
			$graph->get_vertex_attributes( $n ) );
	}
	foreach my $e ( $graph->edges ) {
		my @safe_e = ( _safe_witstr( $e->[0] ), _safe_witstr( $e->[1] ) );
		$safegraph->add_edge( @safe_e );
	}
		
	# Now convert the witness groupings to a safe representation.
	my $safe_groupings = [];
	foreach my $grouping ( @$groupings ) {
		my $safe_grouping = [];
		foreach my $group ( @$grouping ) {
			my $safe_group = [];
			foreach my $n ( @$group ) {
				my $sn = _safe_witstr( $n );
				warn "Ambiguous stringification $sn for $n and " . $witness_map->{$sn}
					if exists $witness_map->{$sn} && $witness_map->{$sn} ne $n;
				$witness_map->{$sn} = $n;
				push( @$safe_group, $sn );
			}
			push( @$safe_grouping, $safe_group );
		}
		push( @$safe_groupings, $safe_grouping );
	}
	
	# Return it all in the struct we expect.  We have stored the reductions
	# in the $witness_map that we were passed.
	return { 'graph' => Text::Tradition::Stemma::editable_graph(
				$safegraph, { 'linesep' => ' ' } ), 
			 'groupings' => $safe_groupings };
}

sub _safe_witstr {
	my $witstr = shift;
	$witstr =~ s/\s+/_/g;
	$witstr =~ s/[^\w\d-]//g;
	return $witstr;
}

sub _desanitize_names {
	my( $element, $witness_map ) = @_;
	my $result = [];
	if( ref( $element ) eq 'ARRAY' ) {
		foreach my $n ( @$element ) {
			push( @$result, _desanitize_names( $n, $witness_map ) );
		}
	} elsif( ref( $element ) eq 'HASH' ) {
		my $real_hash = {};
		map { $real_hash->{$_} = _desanitize_names( $element->{$_}, $witness_map ) }
			keys %$element;
		$result = $real_hash;
	} elsif( exists $witness_map->{$element} ) {
		$result = $witness_map->{$element}
	} else {
		$result = $element;
	}
	return $result;
}

sub _invert_hash {
	my( $hash ) = @_;
	my $newhash;
	foreach my $k ( keys %$hash ) {
		if( ref( $hash->{$k} ) eq 'ARRAY' ) {
			foreach my $v ( @{$hash->{$k}} ) {
				$newhash->{$v} = $k;
			}
		} else {
			$newhash->{$hash->{$k}} = $k;
		}
	}
	return $newhash;
}

### END HACKERY ###

=head2 analyze_location ( $tradition, $graph, $location_hash )

Given the tradition, its stemma graph, and the solution from the graph solver,
work out the rest of the information we want.  For each reading we need missing, 
conflict, reading_parents, independent_occurrence, followed, not_followed,
and follow_unknown.  Alters the location_hash in place.

=cut

sub analyze_location {
	my ( $tradition, $stemma, $variant_row, $lacunose ) = @_;
	my $c = $tradition->collation;
	
	# Make a hash of all known node memberships, and make the subgraphs.
	my $contig = {};
	my $reading_roots = {};
	my $subgraph = {};
	my $acstr = $c->ac_label;
	my @acwits;
	
	my $NO_IDP;
	if( exists $variant_row->{'reading_roots'} ) {
		$reading_roots = delete $variant_row->{'reading_roots'};
	} else {
		warn "No reading source information from IDP - proceed at your own risk";
		$NO_IDP = 1;
	}
	
	# Note which witnesses positively belong to which group. This information
	# comes ultimately from the IDP solver.
	# Also make a note of the reading's roots.
    foreach my $rdghash ( @{$variant_row->{'readings'}} ) {
    	my $rid = $rdghash->{'readingid'};
    	my @roots;
    	foreach my $wit ( @{$rdghash->{'group'}} ) {
    		$contig->{$wit} = $rid;
    	    if( $wit =~ /^(.*)\Q$acstr\E$/ ) {
    	    	push( @acwits, $1 );
    	    }
    	    if( exists $reading_roots->{$wit} && $reading_roots->{$wit} ) {
    	    	push( @roots, $wit );
    	    }
    	}
		$rdghash->{'independent_occurrence'} = \@roots;
	}
	
	# Get the actual graph we should work with
	my $graph;
	try {
		# contig contains all extant wits and all hypothetical wits
		# needed to make up the groups.
		$graph = $stemma->situation_graph( $contig, \@acwits );
	} catch ( Text::Tradition::Error $e ) {
		die "Could not extend graph with given extant and a.c. witnesses: "
			. $e->message;
	} catch {
		die "Could not extend graph with a.c. witnesses @acwits";
	}
	
		
	# Now that we have all the node group memberships, calculate followed/
    # non-followed/unknown values for each reading.  Also figure out the
    # reading's evident parent(s).
    foreach my $rdghash ( @{$variant_row->{'readings'}} ) {
        my $rid = $rdghash->{'readingid'};
        my $rdg = $c->reading( $rid );
        my @roots = @{$rdghash->{'independent_occurrence'}};
        my @group = @{$rdghash->{'group'}};
        
        # Start figuring things out.  
        $rdghash->{'followed'} = scalar( @group ) - scalar( @roots );
        # Find the parent readings, if any, of this reading.
        my $rdgparents = {};
        foreach my $wit ( @roots ) {
        	# Look in the stemma graph to find this witness's extant or known-reading
        	# immediate ancestor(s), and look up the reading that each ancestor olds.
			my @check = $graph->predecessors( $wit );
			while( @check ) {
				my @next;
				foreach my $wparent( @check ) {
					my $preading = $contig->{$wparent};
					if( $preading && $preading ne $rid ) {
						$rdgparents->{$preading} = 1;
					} else {
						push( @next, $graph->predecessors( $wparent ) );
					}
				}
				@check = @next;
			}
		}
		foreach my $p ( keys %$rdgparents ) {
			# Resolve the relationship of the parent to the reading, and
			# save it in our hash.
			my $pobj = $c->reading( $p );
			my $prep = $pobj ? $pobj->id . ' (' . $pobj->text . ')' : $p;
			my $phash = { 'label' => $prep };
			if( $pobj ) {
				my $rel = $c->get_relationship( $p, $rdghash->{readingid} );
				if( $rel ) {
					_add_to_hash( $rel, $phash );
				} elsif( $rdg ) {
					# First check for a transposed relationship
					if( $rdg->rank != $pobj->rank ) {
						foreach my $ti ( $rdg->related_readings( 'transposition' ) ) {
							next unless $ti->text eq $rdg->text;
							$rel = $c->get_relationship( $ti, $pobj );
							if( $rel ) {
								_add_to_hash( $rel, $phash, 1 );
								last;
							}
						}
						unless( $rel ) {
							foreach my $ti ( $pobj->related_readings( 'transposition' ) ) {
								next unless $ti->text eq $pobj->text;
								$rel = $c->get_relationship( $ti, $rdg );
								if( $rel ) {
									_add_to_hash( $rel, $phash, 1 );
									last;
								}
							}
						}
					}
					unless( $rel ) {
						# and then check for sheer word similarity.
						my $rtext = $rdg->text;
						my $ptext = $pobj->text;
						if( similar( $rtext, $ptext ) ) {
							# say STDERR "Words $rtext and $ptext judged similar";
							$phash->{relation} = { type => 'wordsimilar' };
						} 
					}
				} else {
					$phash->{relation} = { type => 'deletion' };
				}
				# Get the attributes of the parent object while we are here
				$phash->{'text'} = $pobj->text if $pobj;
				$phash->{'is_nonsense'} = $pobj->is_nonsense;
				$phash->{'is_ungrammatical'} = $pobj->grammar_invalid;
			} elsif( $p eq '(omitted)' ) {
				$phash->{relation} = { type => 'addition' };
			}
			# Save it
			$rdgparents->{$p} = $phash;
		}
			
		$rdghash->{'reading_parents'} = $rdgparents;
		
		# Find the number of times this reading was altered, and the number of
		# times we're not sure.
		my( %nofollow, %unknownfollow );
		foreach my $wit ( @{$rdghash->{'group'}} ) {
			foreach my $wchild ( $graph->successors( $wit ) ) {
				if( $reading_roots->{$wchild} && $contig->{$wchild}
					&& $contig->{$wchild} ne $rid ) {
					# It definitely changed here.
					$nofollow{$wchild} = 1;
				} elsif( !($contig->{$wchild}) ) {
					# The child is a hypothetical node not definitely in
					# any group. Answer is unknown.
					$unknownfollow{$wchild} = 1;
				} # else it is either in our group, or it is a non-root node in a 
				  # known group and therefore is presumed to have its reading from 
				  # its group, not this link.
			}
		}
		$rdghash->{'not_followed'} = keys %nofollow;
		$rdghash->{'follow_unknown'} = keys %unknownfollow;
		
		# Now say whether this reading represents a conflict.
		unless( $variant_row->{'genealogical'} ) {
			my @trueroots;
			if( exists $variant_row->{'classes'} ) {
				# We have tested for reversions. Use the information.
				my @reversions;
				foreach my $rdgroot ( @roots ) {
					## TODO This needs IDP to prune itself in order to be
					## correct.
					if( $variant_row->{'classes'}->{$rdgroot} eq 'revert' ) {
						push( @reversions, $rdgroot );
					} else {
						push( @trueroots, $rdgroot );
					}
				}
				$rdghash->{'independent_occurrence'} = \@trueroots;
				$rdghash->{'reversion'} = \@reversions if @reversions;
			} else {
				@trueroots = @roots;
			}
			$rdghash->{'conflict'} = @trueroots != 1;
		}		
    }
}

sub _add_to_hash {
	my( $rel, $phash, $is_transposed ) = @_;
	$phash->{relation} = { type => $rel->type };
	$phash->{relation}->{transposed} = 1 if $is_transposed;
	$phash->{relation}->{annotation} = $rel->annotation
		if $rel->has_annotation;
}

=head2 similar( $word1, $word2 )

Use Algorithm::Diff to get a sense of how close the words are to each other.
This will hopefully handle substitutions a bit more nicely than Levenshtein.

=cut

#!/usr/bin/env perl

sub similar {
	my( $word1, $word2 ) = sort { length($a) <=> length($b) } @_;
	my @let1 = split( '', lc( $word1 ) );
	my @let2 = split( '', lc( $word2 ) );
	my $diff = Algorithm::Diff->new( \@let1, \@let2 );
	my $mag = 0;
	while( $diff->Next ) {
		if( $diff->Same ) {
			# Take off points for longer strings
			my $cs = $diff->Range(1) - 2;
			$cs = 0 if $cs < 0;
			$mag -= $cs;
		} elsif( !$diff->Items(1) ) {
			$mag += $diff->Range(2);
		} elsif( !$diff->Items(2) ) {
			$mag += $diff->Range(1);
		} else {
			# Split the difference for substitutions
			my $c1 = $diff->Range(1) || 1;
			my $c2 = $diff->Range(2) || 1;
			my $cd = ( $c1 + $c2 ) / 2;
			$mag += $cd;
		}
	}
	return ( $mag <= length( $word1 ) / 2 );
}

sub _prune_group {
	my( $group, $graph ) = @_;
	my $relevant = {};
	# Record the existence of the vertices in the group
	map { $relevant->{$_} = 1 } @$group;
	# Make our subgraph
	my $subgraph = $graph->deep_copy;
	map { $subgraph->delete_vertex( $_ ) unless $relevant->{$_} }
		$subgraph->vertices;
	# Now prune and return the remaining vertices.
	_prune_subtree( $subgraph );
	# Return the list of vertices and the list of roots.
	my $pruned_group = [ sort $subgraph->vertices ];
	my $pruned_roots = [ $subgraph->predecessorless_vertices ];
	return( $pruned_group, $pruned_roots );
}

sub _prune_subtree {
	my( $tree ) = @_;
	
	# Delete lacunose witnesses that have no successors
	my @orphan_hypotheticals;
	my $ctr = 0;
	do {
		die "Infinite loop on leaves" if $ctr > 100;
		@orphan_hypotheticals = 
			grep { $tree->get_vertex_attribute( $_, 'class' ) eq 'hypothetical' } 
				$tree->successorless_vertices;
		$tree->delete_vertices( @orphan_hypotheticals );
		$ctr++;
	} while( @orphan_hypotheticals );
	
	# Delete lacunose roots that have a single successor
	my @redundant_root;
	$ctr = 0;
	do {
		die "Infinite loop on roots" if $ctr > 100;
		@redundant_root = 
			grep { $tree->get_vertex_attribute( $_, 'class' ) eq 'hypothetical' 
				   && $tree->successors( $_ ) == 1 } 
				$tree->predecessorless_vertices;
		$tree->delete_vertices( @redundant_root );
		$ctr++;
	} while( @redundant_root );
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
