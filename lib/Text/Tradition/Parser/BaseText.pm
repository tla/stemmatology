package Text::Tradition::Parser::BaseText;

use strict;
use warnings;
use Module::Load;
use Algorithm::Diff;

=head1 NAME

Text::Tradition::Parser::BaseText

=head1 SYNOPSIS

use Text::Tradition::Parser::BaseText qw( merge_base );
merge_base( $graph, 'reference.txt', @apparatus_entries )

=head1 DESCRIPTION

For an overview of the package, see the documentation for the
Text::Tradition::Graph module.

This module is meant for use with certain of the other Parser classes
- whenever a list of variants is given with reference to a base text,
these must be joined into a single collation.  The parser should
therefore make a list of variants and their locations, and BaseText
will join those listed variants onto the reference text.  

=head1 SUBROUTINES

=over

=item B<parse>

parse( $graph, %opts );

Takes an initialized graph and a set of options, which must include:
- 'base' - the base text referenced by the variants
- 'format' - the format of the variant list
- 'data' - the variants, in the given format.

=cut

sub parse {
    my( $tradition, %opts ) = @_;

    my $format_mod = 'Text::Tradition::Parser::' . $opts{'format'};
    load( $format_mod );
    my @apparatus_entries = $format_mod->can('read')->( $opts{'data'} );
    merge_base( $tradition->collation, $opts{'base'}, @apparatus_entries );
}

=item B<merge_base>

merge_base( $graph, 'reference.txt', @apparatus_entries )

Takes three arguments: a newly-initialized Text::Tradition::Graph
object, a text file containing the reference text, and a list of
variants (apparatus entries).  Adds the base text to the graph, and
joins the variants to that.

The list of variants is an array of hash references; each hash takes
the form
 { '_id' => line reference,
   'rdg_0' => lemma reading,
   'rdg_1' => first variant,
   ...  # and so on until all distinct readings are listed
   'WitnessA' => 'rdg_0',
   'WitnessB' => 'rdg_1',
   ...  # and so on until all witnesses are listed with their readings
 }

Any hash key that is not of the form /^rdg_\d+$/ and that does not
begin with an underscore is assumed to be a witness name.  Any 'meta'
information to be passed must be passed in a key with a leading
underscore in its name.

=cut

my $SHORTEND = 20; # Debug var - set this to limit the number of lines parsed

my %base_text_index;
my $edits_required = {};

# edits_required -> wit -> [ { start_idx, end_idx, items } ]

sub merge_base {
    my( $collation, $base_file, @app_entries ) = @_;
    my @base_line_starts = read_base( $base_file, $collation );

    my %all_witnesses;
    my @unwitnessed_lemma_nodes;
    foreach my $app ( @app_entries ) {
	my( $line, $num ) = split( /\./, $app->{_id} );
	# DEBUG with a short graph
	last if $SHORTEND && $line > $SHORTEND;
	# DEBUG for problematic entries
	my $scrutinize = '';
	my $first_line_reading = $base_line_starts[ $line ];
	my $too_far = $base_line_starts[ $line+1 ];
	
	my $lemma = $app->{rdg_0};
	my $seq = 1; 
	# Is this the Nth occurrence of this reading in the line?
	if( $lemma =~ s/(_)?(\d)$// ) {
	    $seq = $2;
	}
	my @lemma_words = split( /\s+/, $lemma );
	
	# Now search for the lemma words within this line.
	my $lemma_start = $first_line_reading;
	my $lemma_end;
	my %seen;
	while( $lemma_start ne $too_far ) {
	    # Loop detection
	    if( $seen{ $lemma_start->name() } ) {
		warn "Detected loop at " . $lemma_start->name() . 
		    ", ref $line,$num";
		last;
	    }
	    $seen{ $lemma_start->name() } = 1;
	    
	    # Try to match the lemma.
	    my $unmatch = 0;
	    print STDERR "Matching " . cmp_str( $lemma_start) . " against " .
		$lemma_words[0] . "...\n"
		if "$line.$num" eq $scrutinize;
	    if( cmp_str( $lemma_start ) eq $lemma_words[0] ) {
		# Skip it if we need a match that is not the first.
		if( --$seq < 1 ) {
		    # Now we have to compare the rest of the words here.
		    if( scalar( @lemma_words ) > 1 ) {
			my $next_reading = 
			    $collation->next_reading( $lemma_start );
			foreach my $w ( @lemma_words[1..$#lemma_words] ) {
			    printf STDERR "Now matching %s against %s\n", 
				    cmp_str($next_reading), $w
				if "$line.$num" eq $scrutinize;
			    if( $w ne cmp_str($next_reading) ) {
				$unmatch = 1;
				last;
			    } else {
				$lemma_end = $next_reading;
				$next_reading = 
				    $collation->next_reading( $lemma_end );
			    }
			}
		    } else {
			$lemma_end = $lemma_start;
		    }
		} else {
		    $unmatch = 1;
		}
	    }
	    last unless ( $unmatch || !defined( $lemma_end ) );
	    $lemma_end = undef;
	    $lemma_start = $collation->next_reading( $lemma_start );
	}
	
	unless( $lemma_end ) {
	    warn "No match found for @lemma_words at $line.$num";
	    next;
	}
	
	# Now we have found the lemma; we will record an 'edit', in
	# terms of a splice operation, for each subsequent reading.
	# We also note which witnesses take the given edit.

	my @lemma_set = $collation->reading_sequence( $lemma_start, 
						      $lemma_end );
	my @reading_sets = [ @lemma_set ];

	# For each reading that is not rdg_0, we create the variant
	# reading nodes, and store the range as an edit operation on
	# the base text.
	my $variant_objects;
	my %pc_seen; # Keep track of mss with explicit post-corr data
	foreach my $k ( grep { /^rdg/ } keys( %$app ) ) {
	    my @mss = grep { $app->{$_} eq $k } keys( %$app );
	    push( @unwitnessed_lemma_nodes, @lemma_set )
		if !@mss && $k eq 'rdg_0';

	    # Keep track of what witnesses we have seen.
	    @all_witnesses{ @mss } = ( 1 ) x scalar( @mss );
	    # Keep track of which witnesses bear corrected readings here.
	    foreach my $m ( @mss ) {
		my $base = _is_post_corr( $m );
		next unless $base;
		$pc_seen{$base} = 1;
	    }
	    next if $k eq 'rdg_0';

	    # TODO don't hardcode the reading split operation
	    my @variant = split( /\s+/, $app->{$k} );
	    @variant = () if $app->{$k} eq '/'; # This is an omission.
	    
	    # Make the variant into a set of readings.
	    my @variant_readings;
	    my $ctr = 0;
	    foreach my $vw ( @variant ) {
		my $vwname = "$k/$line.$num.$ctr"; $ctr++;
		my $vwreading = $collation->add_reading( $vwname );
		$vwreading->text( $vw );
		push( @variant_readings, $vwreading );
	    }

	    $variant_objects->{$k} = { 'mss' => \@mss,
				       'reading' => \@variant_readings,
	    };
	    push( @reading_sets, \@variant_readings );
	}

	# Now collate and collapse the identical readings within the
	# collated sets.  Modifies the reading sets that were passed.
	collate_variants( $collation, @reading_sets );

	# TODO Here would be a very good place to set up relationships
	# between the nodes and the lemma.
	set_relationships( $collation, $app, \@lemma_set, $variant_objects );

	# Now create the splice-edit objects that will be used
	# to reconstruct each witness.

	foreach my $rkey ( keys %$variant_objects ) {
	    # Object is argument list for splice, so:
	    # offset, length, replacements
	    my $edit_object = [ $lemma_start->name,
				scalar( @lemma_set ),
				$variant_objects->{$rkey}->{reading} ];
	    foreach my $ms ( @{$variant_objects->{$rkey}->{mss}} ) {
		# Is this a p.c. entry?
		my $base = _is_post_corr( $ms );
		if( $base ) { # this is a post-corr witness
		    my $pc_key = $base . "_post";
		    _add_hash_entry( $edits_required, $pc_key, $edit_object );
		} else { # this is an ante-corr witness
		    my $pc_key = $ms . "_post";
		    _add_hash_entry( $edits_required, $ms, $edit_object );
		    unless( $pc_seen{$ms} ) {
			# If this witness carries no correction, add this 
			# same object to its post-corrected state.
			_add_hash_entry( $edits_required, $pc_key, 
					 $edit_object );
		    }
		}
	    }
	}
    } # Finished going through the apparatus entries

    # Now make the witness objects, and create their text sequences
    foreach my $w ( grep { $_ !~ /_post$/ } keys %$edits_required ) {
	print STDERR "Creating witness $w\n";
	my $witness_obj = $collation->tradition->add_witness( sigil => $w );
	my $debug = undef; # $w eq 'Vb10';
	my @ante_corr_seq = apply_edits( $collation, $edits_required->{$w}, $debug );
	my @post_corr_seq = apply_edits( $collation, $edits_required->{$w."_post"}, $debug )
	    if exists( $edits_required->{$w."_post"} );

	my @repeated = _check_for_repeated( @ante_corr_seq );
	warn "Repeated elements @repeated in $w a.c."
	    if @repeated;
	@repeated = _check_for_repeated( @post_corr_seq );
	warn "Repeated elements @repeated in $w p.c."
	    if @repeated;

	# Now save these paths in my witness object
	if( @post_corr_seq ) {
	    $witness_obj->path( \@post_corr_seq );
	    $witness_obj->uncorrected_path( \@ante_corr_seq );
	} else {
	    $witness_obj->path( \@ante_corr_seq );
	}
    }

    # Now remove our 'base text' edges, which is to say, the only
    # ones we have created so far.  Also remove any nodes that didn't
    # appear in any witnesses.
    foreach ( $collation->paths() ) {
	$collation->del_path( $_ );
    }
    foreach( @unwitnessed_lemma_nodes ) {
	$collation->del_reading( $_ );
    }

    # Now walk paths and calculate positions.
    my @common_readings = 
	$collation->make_witness_paths();
    $collation->calculate_positions( @common_readings );
}

sub _check_for_repeated {
    my @seq = @_;
    my %unique;
    my @repeated;
    foreach ( @seq ) {
	if( exists $unique{$_->name} ) {
	    push( @repeated, $_->name );
	} else {
	    $unique{$_->name} = 1;
	}
    }
    return @repeated;
}

=item B<read_base>

my @line_beginnings = read_base( 'reference.txt', $collation );

Takes a text file and a (presumed empty) collation object, adds the
words as simple linear readings to the collation, and returns a
list of readings that represent the beginning of lines. This collation
is now the starting point for application of apparatus entries in
merge_base, e.g. from a CSV file or a Classical Text Editor file.

=cut

sub read_base {
    my( $base_file, $collation ) = @_;
    
    # This array gives the first reading for each line.  We put the
    # common starting point in line zero.
    my $last_reading = $collation->start();
    $base_text_index{$last_reading->name} = 0;
    my $lineref_array = [ $last_reading ]; # There is no line zero.

    open( BASE, $base_file ) or die "Could not open file $base_file: $!";
    my $i = 1;
    while(<BASE>) {
	# Make the readings, and connect them up for the base, but
	# also save the first reading of each line in an array for the
	# purpose.
	# TODO use configurable reading separator
	chomp;
	my @words = split;
	my $started = 0;
	my $wordref = 0;
	my $lineref = scalar @$lineref_array;
	last if $SHORTEND && $lineref > $SHORTEND;
	foreach my $w ( @words ) {
	    my $readingref = join( ',', $lineref, ++$wordref );
	    my $reading = $collation->add_reading( $readingref );
	    $reading->text( $w );
	    unless( $started ) {
		push( @$lineref_array, $reading );
		$started = 1;
	    }
	    # Add edge paths in the graph, for easier tracking when
	    # we start applying corrections.  These paths will be
	    # removed when we're done.
	    my $path = $collation->add_path( $last_reading, $reading, 
					     $collation->baselabel );
	    $last_reading = $reading;

	    # Note an array index for the reading, for later correction splices.
	    $base_text_index{$readingref} = $i++;
	}
    }
    close BASE;
    # Ending point for all texts
    my $endpoint = $collation->add_reading( '#END#' );
    $collation->add_path( $last_reading, $endpoint, $collation->baselabel );
    push( @$lineref_array, $endpoint );
    $base_text_index{$endpoint->name} = $i;

    return( @$lineref_array );
}

=item B<collate_variants>

collate_variants( $collation, @reading_ranges )

Given a set of readings in the form 
( lemma_start, lemma_end, rdg1_start, rdg1_end, ... )
walks through each to identify those readings that are identical.  The
collation is a Text::Tradition::Collation object; the elements of
@readings are Text::Tradition::Collation::Reading objects that appear
on the collation graph.

TODO: Handle collapsed and non-collapsed transpositions.

=cut

sub collate_variants {
    my( $collation, @reading_sets ) = @_;

    # Two different ways to do this, depending on whether we want
    # transposed reading nodes to be merged into one (producing a
    # nonlinear, bidirectional graph) or not (producing a relatively
    # linear, unidirectional graph.)
    return $collation->linear ? collate_linearly( @_ )
	: collate_nonlinearly( @_ );
}

sub collate_linearly {
    my( $collation, $lemma_set, @variant_sets ) = @_;

    my @unique;
    push( @unique, @$lemma_set );
    while( @variant_sets ) {
	my $variant_set = shift @variant_sets;
	# Use diff to do this job
	my $diff = Algorithm::Diff->new( \@unique, $variant_set, 
					 {'keyGen' => \&_collation_hash} );
	my @new_unique;
	my %merged;
	while( $diff->Next ) {
	    if( $diff->Same ) {
		# merge the nodes
		my @l = $diff->Items( 1 );
		my @v = $diff->Items( 2 );
		foreach my $i ( 0 .. $#l ) {
		    if( !$merged{$l[$i]->name} ) {
			$collation->merge_readings( $l[$i], $v[$i] );
			$merged{$l[$i]->name} = 1;
		    } else {
			print STDERR "Would have double merged " . $l[$i]->name . "\n";
		    }
		}
		# splice the lemma nodes into the variant set
		my( $offset ) = $diff->Get( 'min2' );
		splice( @$variant_set, $offset, scalar( @l ), @l );
		push( @new_unique, @l );
	    } else {
		# Keep the old unique readings
		push( @new_unique, $diff->Items( 1 ) ) if $diff->Items( 1 );
		# Add the new readings to the 'unique' list
		push( @new_unique, $diff->Items( 2 ) ) if $diff->Items( 2 );
	    }
	}
	@unique = @new_unique;
    }
}

sub collate_nonlinearly {
    my( $collation, $lemma_set, @variant_sets ) = @_;
    
    my @unique;
    push( @unique, @$lemma_set );
    while( @variant_sets ) {
	my $variant_set = shift @variant_sets;
	# Simply match the first reading that carries the same word, so
	# long as that reading has not yet been used to match another
	# word in this variant. That way lies loopy madness.
	my @distinct;
	my %merged;
	foreach my $idx ( 0 .. $#{$variant_set} ) {
	    my $vw = $variant_set->[$idx];
	    my @same = grep { cmp_str( $_ ) eq $vw->label } @unique;
	    my $matched;
	    if( @same ) {
		foreach my $i ( 0 .. $#same ) {
		    unless( $merged{$same[$i]->name} ) {
			print STDERR sprintf( "Merging %s into %s\n", 
					      $vw->name,
					      $same[$i]->name );
			$collation->merge_readings( $same[$i], $vw );
			$merged{$same[$i]->name} = 1;
			$matched = $i;
			$variant_set->[$idx] = $same[$i];
		    }
		}
	    }
	    unless( @same && defined($matched) ) {
		push( @distinct, $vw );
	    }
	}
	push( @unique, @distinct );
    }
}


    
sub _collation_hash {
    my $node = shift;
    return cmp_str( $node );
}

sub set_relationships {
    my( $collation, $app, $lemma, $variants ) = @_;
    foreach my $rkey ( keys %$variants ) {
	my $var = $variants->{$rkey}->{'reading'};
	my $typekey = sprintf( "_%s_type", $rkey );
	my $type = $app->{$typekey};
	
	if( $type =~ /^(inv|tr|rep)$/i ) {
	    # Transposition or repetition: look for nodes with the
	    # same label but different IDs and mark them.
	    $type = 'repetition' if $type =~ /^rep/i;
	    $DB::single = 1 if $type eq 'repetition';
	    my %labels;
	    foreach my $r ( @$lemma ) {
		$labels{cmp_str( $r )} = $r;
	    }
	    foreach my $r( @$var ) {
		if( exists $labels{$r->label} &&
		    $r->name ne $labels{$r->label}->name ) {
		    if( $type eq 'repetition' ) {
			# Repetition
			$collation->add_relationship( $type, $r, $labels{$r->label} );
		    } else {
			# Transposition
			$r->set_identical( $labels{$r->label} );
		    }
		}
	    }
	} elsif( $type =~ /^(gr|sp(el)?)$/i ) {
	    # Grammar/spelling: this can be a one-to-one or one-to-many
	    # mapping.  We should think about merging readings if it is
	    # one-to-many.
	    $type = 'grammatical' if $type =~ /gr/i;
	    $type = 'spelling' if $type =~ /sp/i;
	    $type = 'repetition' if $type =~ /rep/i;
	    if( @$lemma == @$var ) {
		foreach my $i ( 0 .. $#{$lemma} ) {
		    $collation->add_relationship( $type, $var->[$i],
						  $lemma->[$i] );
		}
	    } elsif ( @$lemma > @$var && @$var == 1 ) {
		# Merge the lemma readings into one
		## TODO This is a bad solution. We need a real one-to-many
		##  mapping.
		my $ln1 = shift @$lemma;
		foreach my $ln ( @$lemma ) {
		    $collation->merge_readings( $ln1, $ln, ' ' );
		}
		$lemma = [ $ln1 ];
		$collation->add_relationship( $type, $var->[0], $lemma->[0] );
	    } elsif ( @$lemma < @$var && @$lemma == 1 ) {
		my $vn1 = shift @$var;
		foreach my $vn ( @$var ) {
		    $collation->merge_readings( $vn1, $vn, ' ' );
		}
		$var = [ $vn1 ];
		$collation->add_relationship( $type, $var->[0], $lemma->[0] );
	    } else {
		warn "Cannot set $type relationship on a many-to-many variant";
	    }
	} elsif( $type !~ /^(lex|add|om)$/i ) {
	    warn "Unrecognized type $type";
	}
    }
}
	


sub apply_edits {
    my( $collation, $edit_sequence, $debug ) = @_;
    my @lemma_text = $collation->reading_sequence( $collation->start,
					   $collation->reading( '#END#' ) );
    my $drift = 0;
    foreach my $correction ( @$edit_sequence ) {
	my( $lemma_start, $length, $items ) = @$correction;
	my $offset = $base_text_index{$lemma_start};
	my $realoffset = $offset + $drift;
	if( $debug ||
	    $lemma_text[$realoffset]->name ne $lemma_start ) {
	    my @this_phrase = @lemma_text[$realoffset..$realoffset+$length-1];
	    my @base_phrase;
	    my $i = $realoffset;
	    my $l = $collation->reading( $lemma_start );
	    while( $i < $realoffset+$length ) {
		push( @base_phrase, $l );
		$l = $collation->next_reading( $l );
		$i++;
	    }
	    
	    print STDERR sprintf( "Trying to replace %s (%s) starting at %d " .
				  "with %s (%s) with drift %d\n",
				  join( ' ', map {$_->label} @base_phrase ),
				  join( ' ', map {$_->name} @base_phrase ),
				  $realoffset,
				  join( ' ', map {$_->label} @$items ),
				  join( ' ', map {$_->name} @$items ),
				  $drift,
				  ) if $debug;
				  
	    warn( sprintf( "Should be replacing %s (%s) with %s (%s) " .
			   "but %s (%s) is there instead", 
			   join( ' ', map {$_->label} @base_phrase ),
			   join( ' ', map {$_->name} @base_phrase ),
			   join( ' ', map {$_->label} @$items ),
			   join( ' ', map {$_->name} @$items ),
			   join( ' ', map {$_->label} @this_phrase ),
			   join( ' ', map {$_->name} @this_phrase ),
			   ) )
		if $lemma_text[$realoffset]->name ne $lemma_start;
	}
	splice( @lemma_text, $realoffset, $length, @$items );
	$drift += @$items - $length;
    }
    return @lemma_text;
}
	

# Helper function. Given a witness sigil, if it is a post-correctione
# sigil,return the base witness.  If not, return a false value.
sub _is_post_corr {
    my( $sigil ) = @_;
    if( $sigil =~ /^(.*?)(\s*\(?p\.\s*c\.\)?)$/ ) {
	return $1;
    }
    return undef;
}

sub _add_hash_entry {
    my( $hash, $key, $entry ) = @_;
    if( exists $hash->{$key} ) {
	push( @{$hash->{$key}}, $entry );
    } else {
	$hash->{$key} = [ $entry ];
    }
}


=item B<cmp_str>

Pretend you never saw this method.  Really it needs to not be hardcoded.

=cut

sub cmp_str {
    my( $reading ) = @_;
    my $word = $reading->label();
    $word = lc( $word );
    $word =~ s/\W//g;
    $word =~ s/v/u/g;
    $word =~ s/j/i/g;
    $word =~ s/cha/ca/g;
    $word =~ s/quatuor/quattuor/g;
    $word =~ s/ioannes/iohannes/g;
    return $word;
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
