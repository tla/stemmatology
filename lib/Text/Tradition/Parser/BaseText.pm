package Text::Tradition::Parser::BaseText;

use strict;
use warnings;
use Module::Load;
use TryCatch;
use Text::Tradition::Parser::Util qw( collate_variants cmp_str 
	check_for_repeated add_hash_entry );

=head1 NAME

Text::Tradition::Parser::BaseText

=head1 SYNOPSIS

use Text::Tradition::Parser::BaseText qw( merge_base );
merge_base( $graph, 'reference.txt', @apparatus_entries )

=head1 DESCRIPTION

For an overview of the package, see the documentation for the
Text::Tradition module.

This module is meant for use with certain of the other Parser classes
- whenever a list of variants is given with reference to a base text,
these must be joined into a single collation.  The parser should
therefore make a list of variants and their locations, and BaseText
will join those listed variants onto the reference text.  

=head1 SUBROUTINES

=over

=item B<parse>

parse( $graph, $opts );

Takes an initialized graph and a hashref of options, which must include:
- 'base' - the base text referenced by the variants
- 'format' - the format of the variant list
- 'data' - the variants, in the given format.

=cut

sub parse {
    my( $tradition, $opts ) = @_;

    my $format_mod = 'Text::Tradition::Parser::' . $opts->{'input'};
    load( $format_mod );
    # TODO Handle a string someday if we ever have a format other than KUL
    my @apparatus_entries = $format_mod->can('read')->( $opts );
    merge_base( $tradition->collation, $opts, @apparatus_entries );
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

my $SHORTEND = ''; # Debug var - set this to limit the number of lines parsed

my %base_text_index;
my $edits_required = {};

# edits_required -> wit -> [ { start_idx, end_idx, items } ]

sub merge_base {
    my( $collation, $opts, @app_entries ) = @_;
    my @base_line_starts = read_base( $opts->{'base'}, $collation );

    my %all_witnesses;
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
            if( $seen{ $lemma_start->id() } ) {
                warn "Detected loop at " . $lemma_start->id() . 
                    ", ref $line,$num";
                last;
            }
            $seen{ $lemma_start->id() } = 1;
            
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

            # Keep track of what witnesses we have seen.
            @all_witnesses{ @mss } = ( 1 ) x scalar( @mss );
            # Keep track of which witnesses bear corrected readings here.
            foreach my $m ( @mss ) {
                my $base = _is_post_corr( $m );
                next unless $base;
                $pc_seen{$base} = 1;
            }
            next if $k eq 'rdg_0';

            # Parse the variant into reading tokens.
            # TODO don't hardcode the reading split operation
            my @variant = split( /\s+/, $app->{$k} );
            @variant = () if $app->{$k} eq '/'; # This is an omission.
            
            my @variant_readings;
            my $ctr = 0;
            foreach my $vw ( @variant ) {
                my $vwname = "$k/$line.$num.$ctr"; $ctr++;
                my $vwreading = $collation->add_reading( {
                	'id' => $vwname,
                	'text' => $vw } );
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

        # Record any stated relationships between the nodes and the lemma.
        set_relationships( $collation, $app, \@lemma_set, $variant_objects );

        # Now create the splice-edit objects that will be used
        # to reconstruct each witness.

        foreach my $rkey ( keys %$variant_objects ) {
            # Object is argument list for splice, so:
            # offset, length, replacements
            my $edit_object = [ $lemma_start->id,
                                scalar( @lemma_set ),
                                $variant_objects->{$rkey}->{reading} ];
            foreach my $ms ( @{$variant_objects->{$rkey}->{mss}} ) {
                # Is this a p.c. entry?
                my $base = _is_post_corr( $ms );
                if( $base ) { # this is a post-corr witness
                    my $pc_key = $base . "_post";
                    add_hash_entry( $edits_required, $pc_key, $edit_object );
                } else { # this is an ante-corr witness
                    my $pc_key = $ms . "_post";
                    add_hash_entry( $edits_required, $ms, $edit_object );
                    unless( $pc_seen{$ms} ) {
                        # If this witness carries no correction, add this 
                        # same object to its post-corrected state.
                        add_hash_entry( $edits_required, $pc_key, 
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
        my $debug; #  = $w eq 'Vb11';
        my @ante_corr_seq = apply_edits( $collation, $edits_required->{$w}, $debug );
        my @post_corr_seq = apply_edits( $collation, $edits_required->{$w."_post"}, $debug )
            if exists( $edits_required->{$w."_post"} );

        my @repeated = check_for_repeated( @ante_corr_seq );
        warn "Repeated elements @repeated in $w a.c."
            if @repeated;
        @repeated = check_for_repeated( @post_corr_seq );
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
    # ones we have created so far.  Also remove any unwitnessed
    # lemma nodes (TODO unless we are treating base as witness)
    foreach ( $collation->paths() ) {
        $collation->del_path( $_, $collation->baselabel );
    }

    ### HACKY HACKY Do some one-off path corrections here.
    if( $opts->{'input'} eq 'KUL' ) {
		require 'data/boodts/s158.HACK';
		KUL::HACK::pre_path_hack( $collation );
	}
	
    # Now walk paths and calculate positional rank.
    $collation->make_witness_paths();
    # Now delete any orphaned readings.
	foreach my $r ( $collation->sequence->isolated_vertices ) {
		print STDERR "Deleting unconnected reading $r / " . 
			$collation->reading( $r )->text . "\n";
		$collation->del_reading( $r );
	}
	
    KUL::HACK::post_path_hack( $collation ) if $opts->{'input'} eq 'KUL';
    # Have to check relationship validity at this point, because before that
    # we had no paths.
#     foreach my $rel ( $collation->relationships ) {
#         next unless $rel->equal_rank;
#         unless( Text::Tradition::Collation::relationship_valid( $rel->from, $rel->to ) ) {
#             warn sprintf( "Relationship type %s between %s and %s is invalid, deleting",
#                             $rel->type, $rel->from->id, $rel->to->id );
#         }
#     }
    $collation->calculate_ranks();
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
    my $last_reading = $collation->start;
    $base_text_index{$last_reading->id} = 0;
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
            my $reading = $collation->add_reading( { id => $readingref, text => $w } );
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
    $collation->add_path( $last_reading, $collation->end, $collation->baselabel );
    push( @$lineref_array, $collation->end );
    $base_text_index{$collation->end->id} = $i;

    return( @$lineref_array );
}

sub set_relationships {
    my( $collation, $app, $lemma, $variants ) = @_;
    foreach my $rkey ( keys %$variants ) {
        my $var = $variants->{$rkey}->{'reading'};
        my $type = $app->{sprintf( "_%s_type", $rkey )};
        my $noncorr = $app->{sprintf( "_%s_non_corr", $rkey )};
        my $nonindep = $app->{sprintf( "_%s_non_indep", $rkey )};
        
        my %rel_options = ();
        $rel_options{'non_correctable'} = $noncorr if $noncorr && $noncorr =~ /^\d$/;
        $rel_options{'non_indep'} = $nonindep if $nonindep && $nonindep =~ /^\d$/;
        
        if( $type =~ /^(inv|tr|rep)$/i ) {
            # Transposition or repetition: look for nodes with the
            # same label but different IDs and mark them.
            $type = 'repetition' if $type =~ /^rep/i;
            $rel_options{'type'} = $type;
            $rel_options{'equal_rank'} = undef;
            my %labels;
            foreach my $r ( @$lemma ) {
                $labels{cmp_str( $r )} = $r;
            }
            foreach my $r( @$var ) {
                if( exists $labels{$r->text} &&
                    $r->id ne $labels{$r->text}->id ) {
                    if( $type eq 'repetition' ) {
                        # Repetition
                        try {
                        	$collation->add_relationship( $r, $labels{$r->text}, \%rel_options );
                        } catch( Text::Tradition::Error $e ) {
                        	warn "Could not set repetition relationship $r -> " 
                        		. $labels{$r->text} . ": " . $e->message;
                        }
                    } else {
                        # Transposition
                    	try {
                       		$r->set_identical( $labels{$r->text} );
                        } catch( Text::Tradition::Error $e ) {
                        	warn "Could not set transposition relationship $r -> " 
                        		. $labels{$r->text} . ": " . $e->message;
                        }
                    }
                }
            }
        } elsif( $type =~ /^(gr|sp(el)?)$/i ) {

            # Grammar/spelling/lexical: this can be a one-to-one or
            # one-to-many mapping.  We should think about merging
            # readings if it is one-to-many.

            $type = 'grammatical' if $type =~ /gr/i;
            $type = 'spelling' if $type =~ /sp/i;
            $type = 'repetition' if $type =~ /rep/i;
            # $type = 'lexical' if $type =~ /lex/i;
            $rel_options{'type'} = $type;
            $rel_options{'equal_rank'} = 1;
            if( @$lemma == @$var ) {
                foreach my $i ( 0 .. $#{$lemma} ) {
                	try {
						$collation->add_relationship( $var->[$i], $lemma->[$i],
							\%rel_options );
					} catch( Text::Tradition::Error $e ) {
						warn "Could not set $type relationship " . $var->[$i] . " -> " 
							. $lemma->[$i] . ": " . $e->message;
					}
                } 
            } else {
                # An uneven many-to-many mapping.  Skip for now.
                # We really want to make a segment out of whatever we have.
                # my $lemseg = @$lemma > 1 ? $collation->add_segment( @$lemma ) : $lemma->[0];
                # my $varseg = @$var > 1 ? $collation->add_segment( @$var ) : $var->[0];
                # $collation->add_relationship( $varseg, $lemseg, \%rel_options );
                # if( @$lemma == 1 && @$var == 1 ) {
                #     $collation->add_relationship( $lemma->[0], $var->[0], \%rel_options );
                # }
            }
        } elsif( $type !~ /^(add|om|lex)$/i ) {
            warn "Unrecognized type $type";
        }
    }
}
        


sub apply_edits {
    my( $collation, $edit_sequence, $debug ) = @_;
    my @lemma_text = $collation->reading_sequence( 
    	$collation->start, $collation->end );
    my $drift = 0;
    foreach my $correction ( @$edit_sequence ) {
        my( $lemma_start, $length, $items ) = @$correction;
        my $offset = $base_text_index{$lemma_start};
        my $realoffset = $offset + $drift;
        if( $debug ||
            $lemma_text[$realoffset]->id ne $lemma_start ) {
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
                                  join( ' ', map {$_->text} @base_phrase ),
                                  join( ' ', map {$_->id} @base_phrase ),
                                  $realoffset,
                                  join( ' ', map {$_->text} @$items ),
                                  join( ' ', map {$_->id} @$items ),
                                  $drift,
                                  ) if $debug;
                                  
            if( $lemma_text[$realoffset]->id ne $lemma_start ) {
                warn( sprintf( "Should be replacing %s (%s) with %s (%s) " .
                               "but %s (%s) is there instead", 
                               join( ' ', map {$_->text} @base_phrase ),
                               join( ' ', map {$_->id} @base_phrase ),
                               join( ' ', map {$_->text} @$items ),
                               join( ' ', map {$_->id} @$items ),
                               join( ' ', map {$_->text} @this_phrase ),
                               join( ' ', map {$_->id} @this_phrase ),
                      ) );
                # next;
            }
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


=back

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews, aurum@cpan.org

=cut

1;
