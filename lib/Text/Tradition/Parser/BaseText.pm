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

my $SHORT = undef;  # Debug var - set this to limit the number of lines parsed

my %base_text_index;
my $edits_required;

# edits_required -> wit -> [ { start_idx, end_idx, items } ]

sub merge_base {
    my( $collation, $base_file, @app_entries ) = @_;
    my @base_line_starts = read_base( $base_file, $collation );

    my %all_witnesses;
    foreach my $app ( @app_entries ) {
	my( $line, $num ) = split( /\./, $app->{_id} );
	# DEBUG with a short graph
	last if $SHORT && $line > $SHORT;
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

	my @lemma_set = $collation->reading_sequence( $lemma_start, $lemma_end );
	my @reading_sets = [ @lemma_set ];

	# For each reading that is not rdg_0, we create the variant
	# reading nodes, and store the range as an edit operation on
	# the base text.
	my $variant_objects;
	my %pc_lemma; # Keep track of mss that have been corrected back to lemma
	my %pc_variant; # Keep track of mss with other corrections
	foreach my $k ( grep { /^rdg/ } keys( %$app ) ) {
	    my @mss = grep { $app->{$_} eq $k } keys( %$app );
	    # Keep track of what witnesses we have seen.
	    @all_witnesses{ @mss } = ( 1 ) x scalar( @mss );
	    my $pc_hash = $k eq 'rdg_0' ? \%pc_lemma : \%pc_variant;

	    # Keep track of which witnesses bear corrected readings here.
	    foreach my $m ( @mss ) {
		my $base = _is_post_corr( $m );
		next unless $base;
		$pc_hash->{$base} = 1;
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

	# Now create the splice-edit objects that will be used
	# to reconstruct each witness.

	foreach my $rkey ( keys %$variant_objects ) {
	    # Object is argument list for splice, so:
	    # offset, length, replacements
	    my $edit_object = [ $base_text_index{$lemma_start->name},
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
		    _add_hash_entry( $edits_required, $_, $edit_object );
		    unless( !$pc_lemma{$ms} && !$pc_variant{$ms} ) {
			# If this witness carries no correction, add this same object
			# to its post-corrected state.
			# TODO combine these hashes?
			_add_hash_entry( $edits_required, $pc_key, $edit_object );
		    }
		}
	    }
	}
    } # Finished going through the apparatus entries

    # Now make the witness objects, and create their text sequences
    foreach my $w ( grep { $_ !~ /_base$/ } keys %$edits_required ) {
	my $witness_obj = $collation->tradition->add_witness( sigil => $w );
	my @ante_corr_seq = apply_edits( $edits_required->{$w} );
	my @post_corr_seq = apply_edits( $edits_required->{$w."_post"} )
	    if exists( $edits_required->{$w."_post"} );

	# Now how to save these paths in my witness object?
	if( @post_corr_seq ) {
	    $witness_obj->add_path( @post_corr_seq );
	    $witness_obj->add_uncorrected_path( @ante_corr_seq );
	} else {
	    $witness_obj->add_path( @ante_corr_seq );
	}
    }

    # TODO Now remove all the 'base text' links.

    # Now walk paths and calculate positions.
    my @common_readings = 
	$collation->walk_and_expand_base( $collation->reading( '#END#' ) );
    $collation->calculate_positions( @common_readings );
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
    my $lineref_array = [ $last_reading ]; # There is no line zero.

    open( BASE, $base_file ) or die "Could not open file $base_file: $!";
    my $i = 0;
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
	last if $SHORT && $lineref > $SHORT;
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
    # my $detranspose = 1;  # TODO handle merging transposed nodes

    # Merge the nodes across the sets so that there is only one node
    # for any given reading.  Use diff to identify the 'same' nodes.

    my $lemma_set = shift @reading_sets;

    my @unique;
    push( @unique, @$lemma_set );

    while( @reading_sets ) {
	my $variant_set = shift @reading_sets;
	my $diff = Algorithm::Diff->new( \@unique, $variant_set, \&_collation_hash );
	my @new_unique;
	push( @new_unique, @unique );
	while( $diff->Next ) {
	    if( $diff->Same ) {
		# merge the nodes
		my @l = $diff->Items( 1 );
		my @v = $diff->Items( 2 );
		foreach my $i ( 0 .. $#l ) {
		    $collation->merge_readings( $l[$i], $v[$i] );
		}
		# splice the lemma nodes into the variant set
		splice( @$variant_set, $diff->Get( 'min2' ), scalar( @l ), @l );
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

    return;
}

    
sub _collation_hash {
    my $node = shift;
    return _cmp_str( $node->label );
}

sub apply_edits {
    my $edit_sequence = shift;
    my @lemma_text = map { $base_text_index{$_} } sort( keys %base_text_index );

    my $drift = 0;
    foreach my $correction ( @$edit_sequence ) {
	my( $offset, $length, $items ) = @$correction;
	my $realoffset = $offset + $drift;
	splice( @lemma_text, $realoffset, $length, @$items );
	$drift += @$items - $length;
    }
    return \@lemma_text;
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
