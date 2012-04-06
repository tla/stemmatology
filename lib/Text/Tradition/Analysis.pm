package Text::Tradition::Analysis;

use strict;
use warnings;
use Benchmark;
use Encode qw/ encode_utf8 /;
use Exporter 'import';
use Graph;
use JSON qw/ encode_json decode_json /;
use LWP::UserAgent;
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
foreach my $row ( @{$data->{'variants'}} ) {
	# Account for rows that used to be "not useful"
	unless( exists $expected_genealogical{$row->{'id'}} ) {
		$expected_genealogical{$row->{'id'}} = 1;
	}
	is( $row->{'genealogical'}, $expected_genealogical{$row->{'id'}}, 
		"Got correct genealogical flag for row " . $row->{'id'} );
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

	# Figure out which witnesses we are working with
	my @lacunose = $stemma->hypotheticals;
	my @tradition_wits = map { $_->sigil } $tradition->witnesses;
	map { push( @tradition_wits, $_->sigil.$c->ac_label ) if $_->is_layered } 
		$tradition->witnesses;
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
	foreach my $rank ( @ranks ) {
		$DB::single = 1 if $rank == 1003;
		my $missing = [ @lacunose ];
		my $rankgroup = group_variants( $tradition, $rank, $missing, \@collapse );
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
	# Parse the answer
	my $answer = solve_variants( $stemma, @groups );

	# Do further analysis on the answer
	my $conflict_count = 0;
	foreach my $idx ( 0 .. $#use_ranks ) {
		my $location = $answer->{'variants'}->[$idx];
		# Add the rank back in
		$location->{'id'} = $use_ranks[$idx];
		# Add the lacunae back in
		$location->{'missing'} = $lacunae{$use_ranks[$idx]};
		my %lmiss;
		map { $lmiss{$_} = 1 } @{$location->{'missing'}};
		# Run the extra analysis we need.
		analyze_location( $tradition, $stemma->graph, $location );
		foreach my $rdghash ( @{$location->{'readings'}} ) {
			$conflict_count++ 
				if exists $rdghash->{'conflict'} && $rdghash->{'conflict'};
			# Add the reading text back in
			my $rdg = $c->reading( $rdghash->{'readingid'} );
			$rdghash->{'text'} = $rdg ? $rdg->text : $rdghash->{'readingid'};
			# Remove lacunose witnesses from this reading's list now that the
			# analysis is done
			my @realgroup;
			map { push( @realgroup, $_ ) unless $lmiss{$_} } $rdghash->{'group'};
			$rdghash->{'group'} = \@realgroup;
		}
	}
	$answer->{'conflict_count'} = $conflict_count;
	
	return $answer;
}

=head2 group_variants( $tradition, $rank, $lacunose, @merge_relationship_types )

Groups the variants at the given $rank of the collation, treating any
relationships in @merge_relationship_types as equivalent.  $lacunose should
be a reference to an array, to which the sigla of lacunose witnesses at this 
rank will be appended.

Returns a hash $group_readings where $rdg is attested by the witnesses listed 
in $group_readings->{$rdg}.

=cut

# Return group_readings, groups, lacunose
sub group_variants {
	my( $tradition, $rank, $lacunose, $collapse ) = @_;
	my $c = $tradition->collation;
	my $aclabel =  $c->ac_label;
	# Get the alignment table readings
	my %readings_at_rank;
	my %is_lacunose; # lookup table for $lacunose
	map { $is_lacunose{$_} = 1 } @$lacunose;
	my @gap_wits;
	foreach my $tablewit ( @{$c->alignment_table->{'alignment'}} ) {
		my $rdg = $tablewit->{'tokens'}->[$rank-1];
		my $wit = $tablewit->{'witness'};
		# Exclude the witness if it is "lacunose" which if we got here
		# means "not in the stemma".
		next if $is_lacunose{$wit};
		if( $rdg && $rdg->{'t'}->is_lacuna ) {
			_add_to_witlist( $wit, $lacunose, $aclabel );
		} elsif( $rdg ) {
			$readings_at_rank{$rdg->{'t'}->text} = $rdg->{'t'};
		} else {
			_add_to_witlist( $wit, \@gap_wits, $aclabel );
		}
	}
	
	# Group the readings, collapsing groups by relationship if needed
	my %grouped_readings;
	foreach my $rdg ( sort { $b->witnesses <=> $a->witnesses } 
						   values %readings_at_rank ) {
		# Skip readings that have been collapsed into others.
		next if exists $grouped_readings{$rdg->id} && !$grouped_readings{$rdg->id};
		my @wits = $rdg->witnesses;
		if( $collapse ) {
			my $filter = sub { my $r = $_[0]; grep { $_ eq $r->type } @$collapse; };
			foreach my $other ( $rdg->related_readings( $filter ) ) {
				my @otherwits = $other->witnesses;
				push( @wits, @otherwits );
				$grouped_readings{$other->id} = 0;
			}
		}
		my @use_wits = grep { !$is_lacunose{$_} } @wits;
		$grouped_readings{$rdg->id} = \@use_wits;	
	}
	$grouped_readings{'(omitted)'} = \@gap_wits if @gap_wits;
	# Get rid of our collapsed readings
	map { delete $grouped_readings{$_} unless $grouped_readings{$_} } 
		keys %grouped_readings 
		if $collapse;
	
	return \%grouped_readings;
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

	# Make the json with stemma + groups
	my $groupings = [];
	foreach my $ghash ( @groups ) {
		my @grouping;
		foreach my $k ( keys %$ghash ) {
			push( @grouping, $ghash->{$k} );
		}
		push( @$groupings, \@grouping );
	}
	## Witness map is a HACK to get around limitations in node names from IDP
	my $witness_map = {};
	my $json = encode_json( _safe_wit_strings( $stemma, $groupings, $witness_map ) );

	# Send it off and get the result
	my $solver_url = 'http://byzantini.st/cgi-bin/graphcalc.cgi';
	my $ua = LWP::UserAgent->new();
	my $resp = $ua->post( $solver_url, 'Content-Type' => 'application/json', 
						  'Content' => $json );
						  
	my $answer;
	my $used_idp;
	if( $resp->is_success ) {
		$answer = _desanitize_names( decode_json( $resp->content ), $witness_map );
		$used_idp = 1;
	} else {
		# Fall back to the old method.
		warn "IDP solver returned " . $resp->status_line . " / " . $resp->content
			. "; falling back to perl method";
		$answer = perl_solver( $stemma, @$groupings );
	}
	
	# Fold the result back into what we know about the groups.
	my $variants = [];
	my $genealogical = 0;
	foreach my $idx ( 0 .. $#groups ) {
		my( $calc_groups, $result ) = @{$answer->[$idx]};
		if( $result ) {
			$genealogical++;
			# Prune the calculated groups, in case the IDP solver failed to.
			if( $used_idp ) {
				my @pruned_groups;
				foreach my $cg ( @$calc_groups ) {
					my @pg = _prune_group( $cg, $stemma );
					push( @pruned_groups, \@pg );
				}
				$calc_groups = \@pruned_groups;
			}
		}
		my $input_group = $groups[$idx];
		foreach my $k ( keys %$input_group ) {
			my $cg = shift @$calc_groups;
			$input_group->{$k} = $cg;
		}
		my $vstruct = { 
			'genealogical' => $result,
			'readings' => [],
		};
		foreach my $k ( sort { @{$input_group->{$b}} <=> @{$input_group->{$a}} }
							keys %$input_group ) {
			push( @{$vstruct->{'readings'}}, 
				  { 'readingid' => $k, 'group' => $input_group->{$k}} );
		}
		push( @$variants, $vstruct );
	}
	
	return { 'variants' => $variants, 
			 'variant_count' => scalar @$variants,
			 'genealogical_count' => $genealogical };
}

#### HACKERY to cope with IDP's limited idea of what a node name looks like ###

sub _safe_wit_strings {
	my( $stemma, $groupings, $witness_map ) = @_;
	my $safegraph = Graph->new();
	# Convert the graph to a safe representation and store the conversion.
	foreach my $n ( $stemma->graph->vertices ) {
		my $sn = _safe_witstr( $n );
		warn "Ambiguous stringification $sn for $n and " . $witness_map->{$sn}
			if exists $witness_map->{$sn};
		$witness_map->{$sn} = $n;
		$safegraph->add_vertex( $sn );
		$safegraph->set_vertex_attributes( $sn, 
			$stemma->graph->get_vertex_attributes( $n ) );
	}
	foreach my $e ( $stemma->graph->edges ) {
		my @safe_e = ( _safe_witstr( $e->[0] ), _safe_witstr( $e->[1] ) );
		$safegraph->add_edge( @safe_e );
	}
	my $safe_stemma = Text::Tradition::Stemma->new( 
		'collation' => $stemma->collation, 'graph' => $safegraph );
		
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
	return { 'graph' => $safe_stemma->editable( ' ' ), 'groupings' => $safe_groupings };
}

sub _safe_witstr {
	my $witstr = shift;
	$witstr =~ s/\s+/_/g;
	$witstr =~ s/[^\w\d-]//g;
	return $witstr;
}

sub _desanitize_names {
	my( $jsonstruct, $witness_map ) = @_;
	my $result = [];
	foreach my $grouping ( @$jsonstruct ) {
		my $real_grouping = [];
		foreach my $element ( @$grouping ) {
			if( ref( $element ) eq 'ARRAY' ) {
				# it's the groupset.
				my $real_groupset = [];
				foreach my $group ( @$element ) {
					my $real_group = [];
					foreach my $n ( @$group ) {
						my $rn = $witness_map->{$n};
						push( @$real_group, $rn );
					}
					push( @$real_groupset, $real_group );
				}
				push( @$real_grouping, $real_groupset );
			} else {
				# It is the boolean, not actually a group.
				push( @$real_grouping, $element );
			}
		}
		push( @$result, $real_grouping );
	}
	return $result;
}

### END HACKERY ###

=head2 analyze_location ( $tradition, $graph, $location_hash )

Given the tradition, its stemma graph, and the solution from the graph solver,
work out the rest of the information we want.  For each reading we need missing, 
conflict, reading_parents, independent_occurrence, followed, not_followed, and follow_unknown.  Alters the location_hash in place.

=cut

sub analyze_location {
	my ( $tradition, $graph, $variant_row ) = @_;
	
	# Make a hash of all known node memberships, and make the subgraphs.
	my $contig = {};
	my $reading_roots = {};
	my $subgraph = {};
    foreach my $rdghash ( @{$variant_row->{'readings'}} ) {
    	my $rid = $rdghash->{'readingid'};
		map { $contig->{$_} = $rid } @{$rdghash->{'group'}};
        
        # Make the subgraph.
        my $part = $graph->copy;
		my %these_vertices;
		map { $these_vertices{$_} = 1 } @{$rdghash->{'group'}};
		$part->delete_vertices( grep { !$these_vertices{$_} } $part->vertices );
		$subgraph->{$rid} = $part;
		# Get the reading roots.
		map { $reading_roots->{$_} = $rid } $part->predecessorless_vertices;
	}
	
	# Now that we have all the node group memberships, calculate followed/
    # non-followed/unknown values for each reading.  Also figure out the
    # reading's evident parent(s).
    foreach my $rdghash ( @{$variant_row->{'readings'}} ) {
    	# Group string key - TODO do we need this?
        my $gst = wit_stringify( $rdghash->{'group'} );
        my $rid = $rdghash->{'readingid'};
        # Get the subgraph
        my $part = $subgraph->{$rid};
        
        # Start figuring things out.  
        my @roots = $part->predecessorless_vertices;
        $rdghash->{'independent_occurrence'} = scalar @roots;
        $rdghash->{'followed'} = scalar( $part->vertices ) - scalar( @roots );
        # Find the parent readings, if any, of this reading.
        my %rdgparents;
        foreach my $wit ( @roots ) {
        	# Look in the main stemma to find this witness's extant or known-reading
        	# immediate ancestor(s), and look up the reading that each ancestor olds.
			my @check = $graph->predecessors( $wit );
			while( @check ) {
				my @next;
				foreach my $wparent( @check ) {
					my $preading = $contig->{$wparent};
					if( $preading ) {
						$rdgparents{$preading} = 1;
					} else {
						push( @next, $graph->predecessors( $wparent ) );
					}
				}
				@check = @next;
			}
		}
		$rdghash->{'reading_parents'} = [ keys %rdgparents ];
		
		# Find the number of times this reading was altered, and the number of
		# times we're not sure.
		my( %nofollow, %unknownfollow );
		foreach my $wit ( $part->vertices ) {
			foreach my $wchild ( $graph->successors( $wit ) ) {
				next if $part->has_vertex( $wchild );
				if( $reading_roots->{$wchild} && $contig->{$wchild} ) {
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
		
		# Now say whether this reading represents a conflict.
		unless( $variant_row->{'genealogical'} ) {
			$rdghash->{'conflict'} = @roots != 1;
		}		
    }
}


=head2 perl_solver( $tradition, $rank, $stemma_id, @merge_relationship_types )

** NOTE ** This method should hopefully not be called - it is not guaranteed 
to be correct.  Serves as a backup for the real solver.

Runs an analysis of the given tradition, at the location given in $rank, 
against the graph of the stemma specified in $stemma_id.  The argument 
@merge_relationship_types is an optional list of relationship types for
which readings so related should be treated as equivalent.

Returns a nested array data structure as follows:

 [ [ group_list, is_genealogical ], [ group_list, is_genealogical ] ... ]
 
where the group list is the array of arrays passed in for each element of @groups,
possibly with the addition of hypothetical readings.
 

=cut

sub perl_solver {
	my( $stemma, @groups ) = @_;
	my $graph = $stemma->graph;
	my @answer;
	foreach my $g ( @groups ) {
		push( @answer, _solve_variant_location( $graph, $g ) );
	}
	return \@answer;
}

sub _solve_variant_location {
	my( $graph, $groups ) = @_;
	# Now do the work.	
    my $contig = {};
    my $subgraph = {};
    my $is_conflicted;
    my $conflict = {};

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
        # Copy the graph, and delete all non-members from the new graph.
        my $part = $graph->copy;
        my @group_roots;
        $part->delete_vertices( 
            grep { !ref( $contig->{$_} ) && $contig->{$_} ne $gst } $graph->vertices );
                
        # Now look to see if our group is connected.
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
			my $wit = $g->[0];
			@group_roots = ( $wit );
			foreach my $v ( $part->vertices ) {
				$part->delete_vertex( $v ) unless $v eq $wit;
			}
        }
        
        if( @group_roots > 1 ) {
        	$conflict->{$gst} = 1;
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
        
        
		# Save the relevant subgraph.
		$subgraph->{$gst} = $part;
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
                			$conflict->{$gst} = 1;
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
								$conflict->{$gst} = 1;
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
    
    my $is_genealogical = keys %$conflict ? JSON::false : JSON::true;
	my $variant_row = [ [], $is_genealogical ];
	# Fill in the groupings from $contig.
	foreach my $g ( @$groups ) {
    	my $gst = wit_stringify( $g );
    	my @realgroup = grep { $contig->{$_} eq $gst } keys %$contig;
    	push( @{$variant_row->[0]}, \@realgroup );
    }
    return $variant_row;
}

sub _prune_group {
	my( $group, $stemma ) = @_;
	# Get these into a form prune_subtree will recognize. Make a "contighash"
	my $hypohash = {};
	map { $hypohash->{$_} = 1 } @$group;
	# ...with reference values for hypotheticals.
	map { $hypohash->{$_} = [] } $stemma->hypotheticals;
	# Make our subgraph
	my $subgraph = $stemma->graph->copy;
	map { $subgraph->delete_vertex( $_ ) unless exists $hypohash->{$_} }
		$subgraph->vertices;
	# ...and find the root.
	my( $root ) = $subgraph->predecessorless_vertices;
	# Now prune and return the remaining vertices.
	_prune_subtree( $subgraph, $root, $hypohash );
	return $subgraph->vertices;
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
