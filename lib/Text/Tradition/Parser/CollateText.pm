package Text::Tradition::Parser::CollateText;

use strict;
use warnings;

=head1 NAME

Text::Tradition::Parser::CollateText

=head1 DESCRIPTION

For an overview of the package, see the documentation for the
Text::Tradition module.

This module is meant for use with a set of text files saved from Word docs, 
which originated with the COLLATE collation program.  

=head1 SUBROUTINES

=over

=item B<parse>

parse( $graph, $opts );

Takes an initialized graph and a hashref of options, which must include:
- 'base' - the base text referenced by the variants
- 'format' - the format of the variant list
- 'data' - the variants, in the given format.

=cut

my %ALL_SIGLA;

sub parse {
    my( $tradition, $opts ) = @_;
    # String together the base text.
    my $lineref_hash = read_stone_base( $opts->{'base'}, $tradition->collation );
    # Note the sigla.
    foreach my $sigil ( @{$opts->{'sigla'}} ) {
        $ALL_SIGLA{$sigil} = 1;
        $tradition->add_witness( 'sigil' => $sigil );
    }
    # Now merge on the apparatus entries.
    merge_stone_apparatus( $tradition->collation, $lineref_hash, $opts->{'file'} );
}

=item B<read_stone_base>

my $text_list = read_base( 'reference.txt', $collation );

Takes a text file and a (presumed empty) collation object, adds the words
as simple linear readings to the collation, and returns a hash of texts
with line keys. This collation is now the starting point for application of
apparatus entries in merge_base, e.g. from a CSV file or a Classical Text
Editor file.

The hash is of the form 

 { chapter_name => { line_ref => { start => node, end => node } } }

=cut

sub read_stone_base {
    my( $base_file, $collation ) = @_;
    
    # This array gives the first reading for each line.  We put the
    # common starting point in line zero.
    my $last_reading = $collation->start();
    my $lineref_hash = {};
    my $last_lineref;

    my $curr_text;
    open( BASE, $base_file ) or die "Could not open file $base_file: $!";
    binmode BASE, ':utf8';
    my $i = 1;
    while(<BASE>) {
        # Make the readings, and connect them up for the base, but
        # also save the first reading of each line in a hash for the
        # purpose.
        chomp;
        next if /^\s+$/; # skip blank lines
        s/^(\d)\x{589}/$1:/; # turn Armenian full stops into colons
        if( /^TESTAMENT/ ) {
            # Initialize the base hash for this section.
            $lineref_hash->{$_} = {};
            $curr_text = $lineref_hash->{$_};
            next;
        } 
        my @words = split;
        my $lineref;
        if( /^\d/ ) {
            # The first "word" is a line reference; keep it.
            $lineref = shift @words;
        } else {
            # Assume we are dealing with the title.
            $lineref = 'Title:';
        }
        
        # Now turn the remaining words into readings.
        my $wordref = 0;
        foreach my $w ( @words ) {
            my $readingref = join( ',', $lineref, ++$wordref );
            my $reading = $collation->add_reading( $readingref );
            $reading->text( $w );
            unless( exists $curr_text->{$lineref}->{'start'} ) {
                $curr_text->{$lineref}->{'start'} = $reading;
            }
            # Add edge paths in the graph, for easier tracking when
            # we start applying corrections.  These paths will be
            # removed when we're done.
            my $path = $collation->add_path( $last_reading, $reading, 
                                             $collation->baselabel );
            $last_reading = $reading;
        }
        $curr_text->{$lineref}->{'end'} = $last_reading;
    }

    close BASE;
    # Ending point for all texts
    $collation->add_path( $last_reading, $collation->end, $collation->baselabel );
    return( $lineref_hash );
}

=item B<merge_stone_apparatus>

Read an apparatus as output (presumably) by Collate.  It should be reasonably
regular in form, I hope.  Merge the apparatus variants onto the appropriate 
lemma readings.

=cut

sub merge_stone_apparatus {
    my( $c, $lineref_hash, $file ) = @_;
    
    my $text_apps = {};    
    my $current_text;
    open( APP, $file ) or die "Could not read apparatus file $file";
    binmode APP, ':utf8';
    while( <APP> ) {
        chomp;
        next if /^\s*$/;
        if( /^TESTAMENT/ ) {
            $current_text = $lineref_hash->{$_};
            next;
        }
        
        # Otherwise, the first word of the line is the base text line reference.
        my $i = 0;
        my $lineref;
        if( s/^(\S+)\s+// ) {
            $lineref = $1;
        } else {
            warn "Unrecognized line $_";
        }
        my $baseline = $current_text->{$lineref};
        # The start and end readings for this line are now in $baseline->{start}
        # and $baseline->{end}.
            
        # Now look at the apparatus entries for this line. They are
        # split with |.
        my @apps = split( /\s+\|\s+/ );
        my $rdg_ctr = 0;
        foreach my $app ( @apps ) { 
            my( $lemma, $rest ) = split( /\s+\]\s+/, $app );
            next unless $rest; # Skip lines e.g. 'Chapter 2'
            # Find the lemma reading.
            my( $lemma_start, $lemma_end ) = 
                _find_reading_on_line( $c, $lemma, $baseline );
            unless( $lemma_start && $lemma_end ) {
                print STDERR "Lemma $lemma not found; skipping readings $rest\n";
                next;
            }
            my @lemma_chain = $c->reading_sequence( $lemma_start, $lemma_end );
            
            # Splice in "start" and "end" placeholders on either
            # side of the lemma.
            my ( $rdg_start, $rdg_end ) =
                _add_reading_placeholders( $c, $lemma_start, $lemma_end );
                
            # For each reading, attach it to the lemma.
            my @indiv = split( /   /, $rest );
            my $has_rel = 0;
            my %seen_sigla;
            map { $seen_sigla{$_} = 0 } keys %ALL_SIGLA;
            foreach my $rdg ( @indiv ) {
                # Parse the string.
                my( $words, $sigla, $recurse ) = parse_app_entry( $rdg );
                
                # Do something really very dodgy indeed.
                if( exists $sigla->{'__REL__'} && !$has_rel ) {
                    # Handling this has to be deferred until the end, so push it
                    # back onto @indiv and note that we've done so.
                    $has_rel = 1;
                    push( @indiv, $rdg );
                    next;
                }
                
                my @readings;
                foreach my $rdg_word ( @$words ) {
                    next if $rdg_word =~ /^__/;
                    my $reading_id = $lemma_start->name . '_' . $lemma_end->name 
                        . '/' . $rdg_ctr++;
                    my $reading = $c->add_reading( $reading_id );
                    $reading->text( $rdg_word );
                    push( @readings, $reading );
                }
                
                # Deal with any specials.
                my $lemma_sequence;
                if( @$words && $words->[0] eq '__LEMMA__' ) {
                    $lemma_sequence = [ $lemma_end, $rdg_end ];
                } elsif ( @$words && $words->[0] eq '__TRANSPOSE__' ) {
                    # Hope it is only two or three words in the lemma.
                    # TODO figure out how we really want to handle this
                    @readings = reverse @lemma_chain;
                }
                $lemma_sequence = [ $rdg_start, @lemma_chain, $rdg_end ]
                    unless $lemma_sequence;
                
                # Note which sigla we are actually dealing with.
                if( $sigla->{'__REL__'} ) {
                    delete $sigla->{'__REL__'};
                    map { $sigla->{$_} = 1 } 
                        grep { $seen_sigla{$_} == 0 } keys %seen_sigla;
                } else {
                    map { $seen_sigla{$_} = 1 } keys %$sigla;
                }

                # Now hook up the paths.
                unshift( @readings, $lemma_sequence->[0] );
                push( @readings, $lemma_sequence->[-1] );
                foreach my $i ( 1 .. $#readings ) {
                    if( $recurse->{$i} ) {
                        my( $rwords, $rsig ) = parse_app_entry( $recurse->{$i} );
                        # Get the local "lemma" sequence
                        my $llseq = [ $readings[$i], $readings[$i+1] ];
                        if( $rwords->[0] ne '__LEMMA__' ) {
                            unshift( @$llseq, $readings[$i-1] );
                        } # Otherwise treat it as an addition to the last word
                        # Create the reading nodes in $rwords
                        # TODO Hope we don't meet ~ in a recursion
                        my $local_rdg = [];
                        $DB::single = 1;
                        foreach my $i ( 0 .. $#$rwords ) {
                            next if $i == 0 && $rwords->[$i] =~ /^__/;
                            my $reading_id = $llseq->[0]->text . '_' . 
                                $llseq->[-1]->text . '/' . $i;
                            $DB::single = 1 if $reading_id =~ /ATTACH/;
                            my $reading = $c->add_reading( $reading_id );
                            $reading->text( $rwords->[$i] );
                            push( @$local_rdg, $reading );
                        }
                        unshift( @$local_rdg, $llseq->[0] );
                        push( @$local_rdg, $llseq->[-1] );
                        # Add the path(s) necessary
                        _add_sigil_path( $c, $rsig, $llseq, $local_rdg );
                    }
                }
                _add_sigil_path( $c, $sigla, $lemma_sequence, \@readings );
            } # end processing of $app
        } # end foreach my $app in line
    } # end while <line>
    
    # Now reconcile all the paths in the collation, and delete our
    # temporary anchor nodes.
    expand_all_paths( $c );    
    
    # Finally, calculate the ranks we've got.
    $c->calculate_ranks;
}

sub _find_reading_on_line {
    my( $c, $lemma, $baseline, $prior ) = @_;
    
    # We might want the whole line.
    if( $lemma eq 'totum' ) {
        return( $baseline->{'start'}, $baseline->{'end'} );
    }
    
    my $lemma_start = $baseline->{'start'};
    my $lemma_end;
    my $too_far = $c->next_reading( $baseline->{'end'} );
    my @lemma_words = split( /\s+/, $lemma );
    
    my %seen;
    my $scrutinize = '';   # DEBUG variable
    my ( $lw, $seq ) = _get_seq( $lemma_words[0] );
    while( $lemma_start ne $too_far ) {
        # Loop detection
        if( $seen{ $lemma_start->name() } ) {
            warn "Detected loop at " . $lemma_start->name . " for lemma $lemma";
            last;
        }
        $seen{ $lemma_start->name() } = 1;
        
        # Try to match the lemma.
        # TODO move next/prior reading methods into the reading classes,
        # to make this more self-contained and not need to pass $c.
        my $unmatch = 0;
        print STDERR "Matching ".$lemma_start->text." against $lw...\n" 
            if $scrutinize;
        if( _norm( $lemma_start->text ) eq _norm( $lw ) ) {
            # Skip it if we need a match that is not the first.
            if( --$seq < 1 ) {
                # Now we have to compare the rest of the words here.
                if( scalar( @lemma_words ) > 1 ) {
                    my $next_reading = next_real_reading( $c, $lemma_start );
                    my $wildcard = 0;
                    foreach my $w ( @lemma_words[1..$#lemma_words] ) {
                        if( $w eq '---' ) {
                            $wildcard = 1;
                            next;
                        }
                        if( $wildcard ) {
                            # This should be the word after a --- now, and the
                            # last lemma word.
                            my( $wst, $wend ) = _find_reading_on_line( $c, $w, 
                                $baseline, $lemma_start );
                            warn "Something unexpected" unless $wst eq $wend;
                            $lemma_end = $wend;
                            next;
                        }
                        
                        # If we got this far, there is no wildcard.  We must
                        # match each word in sequence.
                        my( $nlw, $nseq ) = _get_seq( $w );
                        printf STDERR "Now matching %s against %s\n", 
                                $next_reading->text, $nlw
                            if $scrutinize;
                        if( _norm( $nlw ) eq _norm( $next_reading->text ) ) {
                            $lemma_end = $next_reading;
                            $next_reading = $c->next_reading( $lemma_end );
                        } else {
                            $unmatch = 1;
                            last;
                        }
                    }
                } else { # single-word match, easy.
                    $lemma_end = $lemma_start;
                }
            } else { # we need the Nth match and aren't there yet
                $unmatch = 1;
            }
            $unmatch = 1 if $prior && !$seen{$prior->name};
        }
        last unless ( $unmatch || !defined( $lemma_end ) );
        $lemma_end = undef;
        $lemma_start = $c->next_reading( $lemma_start );
    }
    
    unless( $lemma_end ) {
        warn "No match found for @lemma_words";
        return undef;
    }   
    return( $lemma_start, $lemma_end );
}

sub _add_reading_placeholders {
    my( $collation, $lemma_start, $lemma_end ) = @_;
    # We will splice in a 'begin' and 'end' marker on either side of the 
    # lemma, as sort of a double-endpoint attachment in the graph.
    # Note that all of this assumes we have a linear base graph at this
    # point, and no diverging readings on the lemmas.
    
    my $start_node = $collation->prior_reading( $lemma_start );
    unless( $start_node->name =~ /ATTACH/ ) {
        my $sn_id = '#ATTACH_' . $lemma_start->name . '_START#';
        my $prior = $start_node;
        $start_node = $collation->add_reading( $sn_id );
        $start_node->is_meta( 1 );
        $collation->graph->del_edge( $collation->graph->edge( $prior, $lemma_start ) );
        $collation->add_path( $prior, $start_node, $collation->baselabel );
        $collation->add_path( $start_node, $lemma_start, $collation->baselabel );
    }
    
    # Now the converse for the end.
    my $end_node = $collation->next_reading( $lemma_end );
    unless( $end_node->name =~ /ATTACH/ ) {
        my $en_id = '#ATTACH_' . $lemma_end->name . '_END#';
        my $next = $end_node;
        $end_node = $collation->add_reading( $en_id );
        $end_node->is_meta( 1 );
        $collation->graph->del_edge( $collation->graph->edge( $lemma_end, $next ) );
        $collation->add_path( $lemma_end, $end_node, $collation->baselabel );
        $collation->add_path( $end_node, $next, $collation->baselabel );
    }
    
    return( $start_node, $end_node );
}

# Function to parse an apparatus reading string, with reference to no other
# data.  Need to do this separately as readings can include readings (ugh).
# Try to give whatever information we might need, including recursive app
# entries that might need to be parsed.

sub parse_app_entry {
    my( $rdg, ) = @_;
    $rdg =~ s/^\s+//;
    $rdg =~ s/\s+$//;
    next unless $rdg;  # just in case
    my @words = split( /\s+/, $rdg );
    # Zero or more sigils e.g. +, followed by Armenian, 
    # followed by (possibly modified) sigla, followed by 
    # optional : with note.
    my $is_add;
    my $is_omission;
    my $is_transposition;
    my $is_base;
    my $skip;
    my @reading;
    my $reading_sigla = {};
    my $recursed;
    my $sig_regex = join( '|', sort { length $b <=> length $a } keys %ALL_SIGLA );
    while( @words ) {
        my $bit = shift @words;
        if( $bit eq '+' ) {
            $is_add = 1;
        } elsif( $bit eq 'om' ) {
            $is_omission = 1;
        } elsif( $bit eq '~' ) {
            $is_transposition = 1;
        } elsif( $bit =~ /\p{Armenian}/ ) {
            warn "Found text in omission?!" if $is_omission;
            push( @reading, $bit );
        } elsif( $bit eq ':' ) {
            # Stop processing.
            last;
        } elsif( $bit =~ /^\(/ ) { 
            # It's a recursive reading within a reading. Lemmatize what we
            # have so far and grab the extra.
            my @new = ( $bit );
            until( $new[-1] =~ /\)$/ ) {
                push( @new, shift @words );
            }
            my $recursed_reading = join( ' ', @new );
            $recursed_reading =~ s/^\((.*)\)/$1/;
            # This recursive entry refers to the last reading word(s) we
            # saw.  Push its index+1.  We will have to come back to parse
            # it when we are dealing with the main reading.
            # TODO handle () as first element
            # TODO handle - as suffix to add, i.e. make new word
            $recursed->{@reading} = $recursed_reading;
        } elsif( $bit =~ /^($sig_regex)(.*)$/ ) {
            # It must be a sigil.
            my( $sigil, $mod ) = ( $1, $2 );
            if( $mod eq "\x{80}" ) {
                $reading_sigla->{$sigil} = '_PC_';
                $ALL_SIGLA{$sigil} = 2;  # a pre- and post-corr version exists
            } elsif( $mod eq '*' ) {
                $reading_sigla->{$sigil} = '_AC_';
                $ALL_SIGLA{$sigil} = 2;  # a pre- and post-corr version exists
            } else {
                $reading_sigla->{$sigil} = 1 unless $mod; # skip secondhand corrections
            }
        } elsif( $bit eq 'rel' ) {
            # The anti-reading. All sigla except those cited.
            $reading_sigla->{'__REL__'} = 1;
        } elsif( $bit eq 'ed' ) {
            # An emendation. TODO make sure all other sigla appear in readings?
            $skip = 1;
            last;
        } elsif( $bit =~ /transpos/ ) {
            # There are some transpositions not coded rigorously; skip them.
            warn "Found hard transposition in $rdg; fix manually";
            last;
        } else {
            warn "Not sure what to do with bit $bit in $rdg";
            $skip = 1;
            last;
        }
    }
    
    return( [], {}, {} ) if $skip;
    # Transmogrify the reading if necessary.
    unshift( @reading, '__LEMMA__' ) if $is_add;
    unshift( @reading, '__TRANSPOSE__' ) if $is_transposition;
    @reading = () if $is_omission;
    unless( @reading || $is_omission ) {
        # It was just sigla on a line, meaning the base changed. Thus
        # the reading is the lemma.
        unshift( @reading, '__LEMMA__' );
    }
   
    return( \@reading, $reading_sigla, $recursed );  
}

# Add a path for the specified sigla to connect the reading sequence.
# Add an a.c. path to the base sequence if we have an explicitly p.c.
# reading.
# Also handle the paths for sigla we have already added in recursive
# apparatus readings (i.e. don't add a path if one already exists.)

sub _add_sigil_path {
    my( $c, $sigla, $base_sequence, $reading_sequence ) = @_;
    my %skip;
    foreach my $sig ( keys %$sigla ) {
        my $use_sig = $sigla->{$sig} eq '_AC_' ? $sig.$c->ac_label : $sig;
        foreach my $i ( 0 .. $#{$reading_sequence}-1 ) {
            if( $skip{$use_sig} ) {
                next if !_has_prior_reading( $reading_sequence->[$i], $use_sig );
                $skip{$use_sig} = 0;
            }
            if( _has_next_reading( $reading_sequence->[$i], $use_sig ) ) {
                $skip{$use_sig} = 1;
                next;
            }
            $c->add_path( $reading_sequence->[$i], $reading_sequence->[$i+1], $use_sig );
        }
        if( $sigla->{$sig} eq '_PC_') {
            $use_sig = $sig.$c->ac_label;
            foreach my $i ( 0 .. $#{$base_sequence}-1 ) {
                if( $skip{$use_sig} ) {
                    next if !_has_prior_reading( $reading_sequence->[$i], $use_sig );
                    $skip{$use_sig} = 0;
                }
                if( _has_next_reading( $reading_sequence->[$i], $use_sig ) ) {
                    $skip{$use_sig} = 1;
                    next;
                }
                $c->add_path( $base_sequence->[$i], $base_sequence->[$i+1], $use_sig );
            }
        }
    }
}

# Remove all ATTACH* nodes, linking the readings on either side of them.
# Then walk the collation for all witness paths, and make sure those paths
# explicitly exist.  Then delete all the 'base' paths.

sub expand_all_paths { 
    my( $c ) = @_;
    
    # Delete the anchors
    foreach my $anchor ( grep { $_->name =~ /ATTACH/ } $c->readings ) {
        # Map each path to its incoming/outgoing node.
        my %incoming;
        map { $incoming{$_->label} = $_->from } $anchor->incoming();
        my %outgoing;
        map { $outgoing{$_->label} = $_->to } $anchor->outgoing();
        $c->del_reading( $anchor );
        
        # Connect in and out.
        my $aclabel = $c->ac_label;
        foreach my $edge ( keys %incoming ) {
            my $from = $incoming{$edge};
            my $to = $outgoing{$edge};
            if( !$to && $edge =~ /^(.*)\Q$aclabel\E$/ ) {
                $to = $outgoing{$1};
            }
            $to = $outgoing{$c->baselabel} unless $to;
            $DB::single = 1 unless $to;
            warn "Have no outbound base link on " . $anchor->name . "!"
                unless $to;
            $c->add_path( $from, $to, $edge );
            delete $outgoing{$edge} unless $edge eq $c->baselabel;
        }
        foreach my $edge ( keys %outgoing ) {
            my $to = $outgoing{$edge};
            my $from = $incoming{$edge};
            if( !$from && $edge =~ /^(.*)\Q$aclabel\E$/ ) {
                $from = $incoming{$1};
            }
            $from = $incoming{$c->baselabel} unless $from;
            warn "Have no inbound base link on " . $anchor->name . "!"
                unless $from;
            $c->add_path( $from, $to, $edge );
        }
    }
    
    $DB::single = 1;
    # Walk the collation and add paths if necessary
    foreach my $sig ( keys %ALL_SIGLA ) {
        my $wit = $c->tradition->witness( $sig );
        my @path = $c->reading_sequence( $c->start, $c->end, $sig );
        $wit->path( \@path );
        if( $ALL_SIGLA{$sig} > 1 ) {
            my @ac_path = $c->reading_sequence( $c->start, $c->end, 
                                                $sig.$c->ac_label, $sig );
            $wit->uncorrected_path( \@path );
            # a.c. paths are already there by default.
        }
        foreach my $i ( 1 .. $#path ) {
            # If there is no explicit path for this sigil between n-1 and n,
            # add it.
            my @sigedges = grep { $_->label eq $sig } $path[$i]->incoming;
            if( @sigedges ) {
                warn "Found more than one path already for $sig" if @sigedges > 1;
                warn "Would add second path for $sig" 
                    unless $sigedges[0]->from eq $path[$i-1];
                next;
            }
            $c->add_path( $path[$i-1], $path[$i], $sig );
        }
    }
    
    # Delete all baselabel edges
    foreach my $edge ( grep { $_->label eq $c->baselabel } $c->paths ) {
        $c->del_edge( $edge );
    }
    
    # Calculate ranks on graph nodes
    $c->calculate_ranks();
}

sub _get_seq {
    my( $str ) = @_;
    my $seq = 1;
    my $lw = $str;
    if( $str =~ /^(.*)(\d)\x{b0}$/ ) {
        ( $lw, $seq) = ( $1, $2 );
    }
    return( $lw, $seq );
}

# Normalize to lowercase, no punct
sub _norm {
    my( $str ) = @_;
    $str =~ s/[^[:alnum:]]//g;
    return lc( $str );
}

sub _has_next_reading {
    my( $rdg, $sigil ) = @_;
    return grep { $_->label eq $sigil } $rdg->outgoing();
}
sub _has_prior_reading {
    my( $rdg, $sigil ) = @_;
    return grep { $_->label eq $sigil } $rdg->incoming();
}
sub next_real_reading {
    my( $c, $rdg ) = @_;
    while( my $r = $c->next_reading( $rdg ) ) {
        return $r unless $r->is_meta;
        return $r if $r eq $c->end;
        $rdg = $r;
    }
}
# For debugging
sub rstr {
    my @l = @_;
    if( ref( $_[0] ) eq 'ARRAY' ) {
        @l = @$_[0];
    }
    my $str = join( ' ', map { $_->text } @l );
    return $str;
}

1;