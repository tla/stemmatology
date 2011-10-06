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
	
	my $all_wits_table = $tradition->collation->make_alignment_table( 'refs' );
	
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
		# TODO: deal with a.c. reading logic
		$DB::single = 1 if $rank == 25;
		my $variant_row = analyze_variant_location( $group_readings, $groups, 
		    $stemma->apsp, $lacunose );
		$variant_row->{'id'} = $rank;
		$genealogical++ if $variant_row->{'genealogical'};
		$conflicts += grep { $_->{'conflict'} } @{$variant_row->{'readings'}};

		# Now run the same analysis given the calculated distance tree(s).
# 		my @trees = @{$stemma->distance_trees};
# 		if( @trees ) {
#             foreach my $tree ( 0 .. $#trees ) {
#                 my $dc = analyze_variant_location( $group_readings, $groups,    
#                                                    $stemma->distance_apsps->[$tree] );
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
    my( $group_readings, $groups, $apsp, $lacunose ) = @_;
    my %contig;
    my $conflict = {};
    my %missing;
    map { $missing{$_} = 1 } @$lacunose;
    my $variant_row = { 'readings' => [] };
    # Mark each ms as in its own group, first.
    foreach my $g ( @$groups ) {
        my $gst = wit_stringify( $g );
        map { $contig{$_} = $gst } @$g;
    }
    foreach my $g ( sort { scalar @$b <=> scalar @$a } @$groups ) {
        my @members = @$g;
        my $gst = wit_stringify( $g ); # $gst is now the name of this group.
        while( @members ) {
            # Gather the list of vertices that are needed to join all members.
            my $curr = pop @members;
            foreach my $m ( @members ) {
                foreach my $v ( $apsp->path_vertices( $curr, $m ) ) {
                    $contig{$v} = $gst unless exists $contig{$v};
                    next if $contig{$v} eq $gst;
                    # Record what is conflicting. TODO do we use this?
                    $conflict->{$group_readings->{$gst}} = $group_readings->{$contig{$v}};
                }
            }
        }
        # Write the reading.
        my $reading = { 'text' => $group_readings->{$gst},
                        'missing' => wit_stringify( $lacunose ),
                        'conflict' => exists( $conflict->{$group_readings->{$gst}} ) };
        if( $reading->{'conflict'} ) {
            $reading->{'group'} = $gst;
        } else {
            my @all_vertices = grep { $contig{$_} eq $gst && !$missing{$_} } keys %contig;
            $reading->{'group'} = wit_stringify( \@all_vertices );
        }
        push( @{$variant_row->{'readings'}}, $reading );
    }
    $variant_row->{'genealogical'} = keys %$conflict ? undef : 1;
    return $variant_row;
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