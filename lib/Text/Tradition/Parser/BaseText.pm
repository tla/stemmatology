package Text::Tradition::Parser::BaseText;

use strict;
use warnings;
use Module::Load;

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

sub merge_base {
    my( $collation, $base_file, @app_entries ) = @_;
    my @base_line_starts = read_base( $base_file, $collation );

    my %all_witnesses;
    foreach my $app ( @app_entries ) {
	my( $line, $num ) = split( /\./, $app->{_id} );
	# DEBUG with a short graph
	# last if $line > 2;
	# DEBUG for problematic entries
	my $scrutinize = "";
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
	} else {
	    # These are no longer common readings; unmark them as such.
	    my @lemma_readings = $collation->reading_sequence( $lemma_start, 
						     $lemma_end );
	    map { $_->set_attribute( 'class', 'lemma' ) } @lemma_readings;
	}
	
	# Now we have our lemma readings; we add the variant readings
	# to the collation.
	
	# Keep track of the start and end point of each reading for later
	# reading collapse.
	my @readings = ( $lemma_start, $lemma_end );

	# For each reading that is not rdg_0, we make a chain of readings
	# and connect them to the anchor.  Edges are named after the mss
	# that are relevant.
	foreach my $k ( grep { /^rdg/ } keys( %$app ) ) {
	    next if $k eq 'rdg_0'; # that's the lemma.
	    # TODO look at the lemma for any p.c. readings, and add
	    # them explicitly!
	    my @variant = split( /\s+/, $app->{$k} );
	    @variant = () if $app->{$k} eq '/'; # This is an omission.
	    my @mss = grep { $app->{$_} eq $k } keys( %$app );
	    
	    unless( @mss ) {
		print STDERR "Skipping '@variant' at $line.$num: no mss\n";
		next;
	    }
	    
	    # Keep track of what witnesses we have seen.
	    @all_witnesses{ @mss } = ( 1 ) x scalar( @mss );
	    
	    # Make the variant into a set of readings.
	    my $ctr = 0;
	    my $last_reading = $collation->prior_reading( $lemma_start );
	    my $var_start;
	    foreach my $vw ( @variant ) {
		my $vwname = "$k/$line.$num.$ctr"; $ctr++;
		my $vwreading = $collation->add_reading( $vwname );
		$vwreading->text( $vw );
		$vwreading->make_variant();
		foreach ( @mss ) {
		    $collation->add_path( $last_reading, $vwreading, $_ );
		}
		$var_start = $vwreading unless $var_start;
		$last_reading = $vwreading;
	    }
	    # Now hook it up at the end.
	    foreach ( @mss ) {
		$collation->add_path( $last_reading, 
				      $collation->next_word( $lemma_end ),
				      $_ );
	    }
	    
	    if( $var_start ) { # if it wasn't an empty reading
		push( @readings, $var_start, $last_reading );
	    }
	}

	# Now collate and collapse the identical readings within the collation.
	collate_variants( $collation, @readings );
    }

    # Now make the witness objects
    foreach my $w ( keys %all_witnesses ) {
	my $base = _is_post_corr( $w );
	if( $base ) {
	    my $pctag = substr( $w, length( $base ) );
	    my $existing_wit = $collation->tradition->witness( $base );
	    unless( $existing_wit ) {
		$existing_wit = $collation->tradition->add_witness( $base );
	    }
	    $existing_wit->post_correctione( $pctag );
	} else {
	    $collation->tradition->add_witness( $w )
		unless $collation->tradition->witness( $w );
	}
    }

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
	foreach my $w ( @words ) {
	    my $readingref = join( ',', $lineref, ++$wordref );
	    my $reading = $collation->add_reading( $readingref );
	    $reading->text( $w );
	    $reading->make_common();
	    unless( $started ) {
		push( @$lineref_array, $reading );
		$started = 1;
	    }
	    if( $last_reading ) {
		my $path = $collation->add_path( $last_reading, $reading, 
						 "base text" );
		$path->set_attribute( 'class', 'basetext' );
		$last_reading = $reading;
	    } # TODO there should be no else here...
	}
    }
    close BASE;
    # Ending point for all texts
    my $endpoint = $collation->add_reading( '#END#' );
    $collation->add_path( $last_reading, $endpoint, "base text" );
    push( @$lineref_array, $endpoint );

    return( @$lineref_array );
}

=item B<collate_variants>

collate_variants( $collation, @readings )

Given a set of readings in the form 
( lemma_start, lemma_end, rdg1_start, rdg1_end, ... )
walks through each to identify those readings that are identical.  The
collation is a Text::Tradition::Collation object; the elements of
@readings are Text::Tradition::Collation::Reading objects that appear
on the collation graph.

TODO: Handle collapsed and non-collapsed transpositions.

=cut

sub collate_variants {
    my( $collation, @readings ) = @_;
    my $lemma_start = shift @readings;
    my $lemma_end = shift @readings;
    my $detranspose = 0;

    # We need to calculate positions at this point, which is where
    # we are getting the implicit information from the apparatus.

    # Start the list of distinct readings with those readings in the lemma.
    my @distinct_readings;
    my $position = 0;
    while( $lemma_start ne $lemma_end ) {
	push( @distinct_readings, [ $lemma_start, 'base text', $position++ ] );
	$lemma_start = $collation->next_word( $lemma_start );
    } 
    push( @distinct_readings, [ $lemma_end, 'base text', $position++ ] );
    

    while( scalar @readings ) {
	my( $var_start, $var_end ) = splice( @readings, 0, 2 );

	# I want to look at the readings in the variant and lemma, and
	# collapse readings that are the same word.  This is mini-collation.
	# Each word in the 'main' list can only be collapsed once with a
	# word from the current reading.
	my %collapsed = ();

	# Get the label. There will only be one outgoing path to start
	# with, so this is safe.
	my @out = $var_start->outgoing();
	my $var_label = $out[0]->label();

	my @variant_readings;
	while( $var_start ne $var_end ) {
	    push( @variant_readings, $var_start );
	    $var_start = $collation->next_word( $var_start, $var_label );
	}
	push( @variant_readings, $var_end );

	# Go through the variant readings, and if we find a lemma reading that
	# hasn't yet been collapsed with a reading, equate them.  If we do
	# not, keep them to push onto the end of all_readings.
	my @remaining_readings;
	my $last_index = 0;
	my $curr_pos = 0;
	foreach my $w ( @variant_readings ) {
	    my $word = $w->label();
	    my $matched = 0;
	    foreach my $idx ( $last_index .. $#distinct_readings ) {
		my( $l, $pathlabel, $pos ) = @{$distinct_readings[$idx]};
		if( $word eq cmp_str( $l ) ) {
		    next if exists( $collapsed{ $l->label } )
			&& $collapsed{ $l->label } eq $l;
		    $matched = 1;
		    $last_index = $idx if $detranspose;
		    # Collapse the readings.
		    printf STDERR "Merging readings %s/%s and %s/%s\n", 
		        $l->name, $l->label, $w->name, $w->label;
		    $collation->merge_readings( $l, $w );
		    $collapsed{ $l->label } = $l;
		    # Now collapse any multiple paths to and from the reading.
 		    remove_duplicate_paths( $collation, 
 				    $collation->prior_word( $l, $pathlabel ), $l );
 		    remove_duplicate_paths( $collation, $l, 
 				    $collation->next_word( $l, $pathlabel ) );
		    $curr_pos = $pos;
		    last;
		}
	    }
	    push( @remaining_readings, [ $w, $var_label, $curr_pos++ ] ) unless $matched;
	}
	push( @distinct_readings, @remaining_readings) if scalar( @remaining_readings );
    }

    # Now set the positions of all the readings in this variation.
    #$DB::single = 1;
    print STDERR "Readings and their positions are:\n";
    foreach my $n ( @distinct_readings ) {
	printf STDERR "\t%s (position %s)\n", $n->[0]->label(), $n->[2];
    }
}

=item B<remove_duplicate_paths>

remove_duplicate_paths( $collation, $from, $to );

Given two readings, reduce the number of paths between those readings to
one.  If neither path represents a base text, combine their labels.

=cut

sub remove_duplicate_paths {
    my( $collation, $from, $to ) = @_;
    my @paths = $from->paths_to( $to );
    if( scalar @paths > 1 ) {
	my @base = grep { $_->label eq 'base text' } @paths;
	if ( scalar @base ) {
	    # Remove the paths that are not base.
	    foreach my $e ( @paths ) {
		$collation->del_path( $e )
		    unless $e eq $base[0];
	    }
	} else {
	    # Combine the paths into one.
	    my $new_path_name = join( ', ', map { $_->label() } @paths );
	    my $new_path = shift @paths;
	    $new_path->set_attribute( 'label', $new_path_name );
	    foreach my $e ( @paths ) {
		$collation->del_path( $e );
	    }
	}
    }
}

# Helper function. Given a witness sigil, if it is a post-correctione
# sigil,return the base witness.  If not, return a false value.
sub _is_post_corr {
    my( $sigil ) = @_;
    if( $sigil =~ /^(.*?)(\s*\(p\.\s*c\.\))$/ ) {
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
