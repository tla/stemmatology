package Text::Tradition::Parser::TEI;

use strict;
use warnings;
use Text::Tradition::Parser::Util qw( collate_variants );
use XML::LibXML;
use XML::LibXML::XPathContext;

=head1 NAME

Text::Tradition::Parser::TEI

=head1 DESCRIPTION

Parser module for Text::Tradition, given a TEI parallel-segmentation
file that describes a text and its variants.

=head1 METHODS

=over

=item B<parse>

parse( $tei_string );

Takes an initialized tradition and a string containing the TEI;
creates the appropriate nodes and edges on the graph, as well as
the appropriate witness objects.

=cut

my $text = {}; # Hash of arrays, one per eventual witness we find.
my @common_readings;
my $substitutions = {}; # Keep track of merged readings
my $app_anchors = {};   # Track apparatus references
my $app_ac = {};        # Save a.c. readings

# Create the package variables for tag names.

# Would really like to do this with varname variables, but apparently this
# is considered a bad idea.  The long way round then.
my( $LISTWIT, $WITNESS, $TEXT, $W, $SEG, $APP, $RDG, $LEM ) 
    = ( 'listWit', 'witness', 'text', 'w', 'seg', 'app', 'rdg', 'lem' );
sub make_tagnames {
    my( $ns ) = @_;
    if( $ns ) {
        $LISTWIT = "$ns:$LISTWIT";
        $WITNESS = "$ns:$WITNESS";
        $TEXT = "$ns:$TEXT";
        $W = "$ns:$W";
        $SEG = "$ns:$SEG";
        $APP = "$ns:$APP";
        $RDG = "$ns:$RDG";
        $LEM = "$ns:$LEM";
    }
}

# Parse the TEI file.
sub parse {
    my( $tradition, $xml_str ) = @_;
    
    # First, parse the XML.
    my $parser = XML::LibXML->new();
    my $doc = $parser->parse_string( $xml_str );
    my $tei = $doc->documentElement();
    my $xpc = XML::LibXML::XPathContext->new( $tei );
    my $ns;
    if( $tei->namespaceURI ) {
        $ns = 'tei';
        $xpc->registerNs( $ns, $tei->namespaceURI );
    }
    make_tagnames( $ns );

    # Then get the witnesses and create the witness objects.
    foreach my $wit_el ( $xpc->findnodes( "//$LISTWIT/$WITNESS" ) ) {
        my $sig = $wit_el->getAttribute( 'xml:id' );
        my $source = $wit_el->toString();
        $tradition->add_witness( sigil => $sig, source => $source );
    }

    map { $text->{$_->sigil} = [] } $tradition->witnesses;
    # Look for all word/seg node IDs and note their pre-existence.
    my @attrs = $xpc->findnodes( "//$W|$SEG/attribute::xml:id" );
    save_preexisting_nodeids( @attrs );

    # Now go through the children of the text element and pull out the
    # actual text.
    foreach my $xml_el ( $xpc->findnodes( "//$TEXT" ) ) {
        foreach my $xn ( $xml_el->childNodes ) {
            _get_readings( $tradition, $xn );
        }
    }
    # Our $text global now has lists of readings, one per witness.
    # Join them up.
    my $c = $tradition->collation;
    foreach my $sig ( keys %$text ) {
        next if $sig eq 'base';  # Skip base text readings with no witnesses.
        # Determine the list of readings for 
        my $sequence = $text->{$sig};
        my @real_sequence = ( $c->start );
        push( @$sequence, $c->end );
        my $source = $c->start;
        foreach( _clean_sequence( $sig, $sequence ) ) {
            my $rdg = _return_rdg( $_ );
            push( @real_sequence, $rdg );
            $c->add_path( $source, $rdg, $sig );
            $source = $rdg;
        }
        $tradition->witness( $sig )->path( \@real_sequence );
        # See if we need to make an a.c. version of the witness.
        if( exists $app_ac->{$sig} ) {
            my @uncorrected;
            push( @uncorrected, @real_sequence );
            foreach my $app ( keys %{$app_ac->{$sig}} ) {
                my $start = _return_rdg( $app_anchors->{$app}->{$sig}->{'start'} ); 
                my $end = _return_rdg( $app_anchors->{$app}->{$sig}->{'end'} );
                my @new = map { _return_rdg( $_ ) } @{$app_ac->{$sig}->{$app}};
                _replace_sequence( \@uncorrected, $start, $end, @new );
            }
            my $source = $c->start;
            foreach my $rdg ( @uncorrected ) {
                my $has_base = grep { $_->label eq $sig } $source->edges_to( $rdg );
                if( $rdg ne $c->start && !$has_base ) {
                    print STDERR sprintf( "Adding path %s from %s -> %s\n",
                        $sig.$c->ac_label, $source->name, $rdg->name );
                    $c->add_path( $source, $rdg, $sig.$c->ac_label );
                }
                $source = $rdg;
            }
            $tradition->witness( $sig )->uncorrected_path( \@uncorrected );
        }
    }
    # Delete readings that are no longer part of the graph.
    # TODO think this is useless actually
    foreach ( keys %$substitutions ) {
        $tradition->collation->del_reading( $tradition->collation->reading( $_ ) );
    }
    $tradition->collation->calculate_positions( @common_readings );
}

sub _clean_sequence {
    my( $wit, $sequence ) = @_;
    my @clean_sequence;
    foreach my $rdg ( @$sequence ) {
        if( $rdg =~ /^PH-(.*)$/ ) {
            # It is a placeholder.  Keep it only if we need it.
            my $app_id = $1;
            if( exists $app_ac->{$wit}->{$app_id} ) {
                print STDERR "Retaining empty placeholder for $app_id\n";
                push( @clean_sequence, $rdg );
            }
        } else {
            push( @clean_sequence, $rdg );
        }
    }
    return @clean_sequence;
}

sub _replace_sequence {
    my( $arr, $start, $end, @new ) = @_;
    my( $start_idx, $end_idx );
    foreach my $i ( 0 .. $#{$arr} ) {
        $start_idx = $i if( $arr->[$i]->name eq $start );
        if( $arr->[$i]->name eq $end ) {
            $end_idx = $i;
            last;
        }
    }
    unless( $start_idx && $end_idx ) {
        warn "Could not find start and end";
        return;
    }
    my $length = $end_idx - $start_idx + 1;
    splice( @$arr, $start_idx, $length, @new );
}

sub _return_rdg {
    my( $rdg ) = @_;
    # If we were passed a reading name, return the name.  If we were
    # passed a reading object, return the object.
    my $wantobj = ref( $rdg ) eq 'Text::Tradition::Collation::Reading';
    my $real = $rdg;
    if( exists $substitutions->{ $wantobj ? $rdg->name : $rdg } ) {
        $real = $substitutions->{ $wantobj ? $rdg->name : $rdg };
        $real = $real->name unless $wantobj;
    }
    return $real;
}

## Recursive helper function to help us navigate through nested XML,
## picking out the text.  $tradition is the tradition, needed for
## making readings; $xn is the XML node currently being looked at,
## $in_var is a flag to say that we are inside a variant, $ac is a
## flag to say that we are inside an ante-correctionem reading, and
## @cur_wits is the list of witnesses to which this XML node applies.
## Returns the list of readings, if any, created on the run.

{
    my @active_wits;
    my $current_app;

    sub _get_readings {
        my( $tradition, $xn, $in_var, $ac, @cur_wits ) = @_;
        @cur_wits = @active_wits unless $in_var;

        my @new_readings;
        if( $xn->nodeType == XML_TEXT_NODE ) {
            # Some words, thus make some readings.
            my $str = $xn->data;
            return unless $str =~ /\S/; # skip whitespace-only text nodes
            #print STDERR "Handling text node " . $str . "\n";
            # Check that all the witnesses we have are active.
            foreach my $c ( @cur_wits ) {
                warn "Could not find $c in active wits"
                    unless grep { $c eq $_ } @active_wits;
            }
            $str =~ s/^\s+//;
            my $final = $str =~ s/\s+$//;
            foreach my $w ( split( /\s+/, $str ) ) {
                # For now, skip punctuation.
                next if $w !~ /[[:alnum:]]/;
                my $rdg = make_reading( $tradition->collation, $w );
                push( @new_readings, $rdg );
                unless( $in_var ) {
                    push( @common_readings, $rdg );
                    $rdg->make_common;
                }
                foreach ( @cur_wits ) {
                    warn "Empty wit!" unless $_;
                    warn "Empty reading!" unless $rdg;
                    push( @{$text->{$_}}, $rdg ) unless $ac;
                }
            }
        } elsif( $xn->nodeName eq 'w' ) {
            # Everything in this tag is one word.  Also save any original XML ID.
            #print STDERR "Handling word " . $xn->toString . "\n";
            # Check that all the witnesses we have are active.
            foreach my $c ( @cur_wits ) {
                warn "Could not find $c in active wits"
                    unless grep { $c eq $_ } @active_wits;
            }
            my $xml_id = $xn->getAttribute( 'xml:id' );
            my $rdg = make_reading( $tradition->collation, $xn->textContent, $xml_id );
            push( @new_readings, $rdg );
            unless( $in_var ) {
                push( @common_readings, $rdg );
                $rdg->make_common;
            }
            foreach( @cur_wits ) {
                warn "Empty wit!" unless $_;
                warn "Empty reading!" unless $rdg;
                push( @{$text->{$_}}, $rdg ) unless $ac;
            }
        } elsif ( $xn->nodeName eq 'app' ) {
            $current_app = $xn->getAttribute( 'xml:id' );
            # print STDERR "Handling app $current_app\n";
            # Keep the reading sets in this app.
            my @sets;
            # Recurse through all children (i.e. rdgs) for sets of words.
            foreach ( $xn->childNodes ) {
                my @rdg_set = _get_readings( $tradition, $_, $in_var, $ac, @cur_wits );
                push( @sets, \@rdg_set ) if @rdg_set;
            }
            # Now collate these sets if we have more than one.
            my $subs = collate_variants( $tradition->collation, @sets ) if @sets > 1;
            map { $substitutions->{$_} = $subs->{$_} } keys %$subs;
            # TODO Look through substitutions to see if we can make anything common now.
            # Return the entire set of unique readings.
            my %unique;
            foreach my $s ( @sets ) {
                map { $unique{$_->name} = $_ } @$s;
            }
            push( @new_readings, values( %unique ) );
            # Exit the current app.
            $current_app = '';
        } elsif ( $xn->nodeName eq 'lem' || $xn->nodeName eq 'rdg' ) {
            # Alter the current witnesses and recurse.
            #print STDERR "Handling reading for " . $xn->getAttribute( 'wit' ) . "\n";
            $ac = $xn->getAttribute( 'type' ) && $xn->getAttribute( 'type' ) eq 'a.c.';
            my @rdg_wits = get_sigla( $xn );
            @rdg_wits = ( 'base' ) unless @rdg_wits;  # Allow for editorially-supplied readings
            my @words;
            foreach ( $xn->childNodes ) {
                my @rdg_set = _get_readings( $tradition, $_, 1, $ac, @rdg_wits );
                push( @words, @rdg_set ) if @rdg_set;
            }
            # If we have more than one word in a reading, it should become a segment.
            # $tradition->collation->add_segment( @words ) if @words > 1;
            
            if( $ac ) {
                # Add the reading set to the a.c. readings.
                foreach ( @rdg_wits ) {
                    $app_ac->{$_}->{$current_app} = \@words;
                }
            } else {
                # Add the reading set to the app anchors for each witness
                # or put in placeholders for empty p.c. readings
                foreach ( @rdg_wits ) {
                    my $start = @words ? $words[0]->name : "PH-$current_app";
                    my $end = @words ? $words[-1]->name : "PH-$current_app";
                    $app_anchors->{$current_app}->{$_}->{'start'} = $start;
                    $app_anchors->{$current_app}->{$_}->{'end'} = $end;
                    push( @{$text->{$_}}, $start ) unless @words;
                }
            }
            push( @new_readings, @words );
        } elsif( $xn->nodeName eq 'witStart' ) {
            # Add the relevant wit(s) to the active list.
            #print STDERR "Handling witStart\n";
            push( @active_wits, @cur_wits );
        } elsif( $xn->nodeName eq 'witEnd' ) {
            # Take the relevant wit(s) out of the list.
            #print STDERR "Handling witEnd\n";
            my $regexp = '^(' . join( '|', @cur_wits ) . ')$';
            @active_wits = grep { $_ !~ /$regexp/ } @active_wits;
        } elsif( $xn->nodeName eq 'witDetail' ) {
            # Ignore these for now.
            return;
        } else {
            # Recurse as if this tag weren't there.
            #print STDERR "Recursing on tag " . $xn->nodeName . "\n";
            foreach( $xn->childNodes ) {
                push( @new_readings, _get_readings( $tradition, $_, $in_var, $ac, @cur_wits ) );
            }
        }
        return @new_readings;
    }

}

# Helper to extract a list of witness sigla from a reading element.
sub get_sigla {
    my( $rdg ) = @_;
    # Cope if we have been handed a NodeList.  There is only
    # one reading here.
    if( ref( $rdg ) eq 'XML::LibXML::NodeList' ) {
        $rdg = $rdg->shift;
    }

    my @wits;
    if( ref( $rdg ) eq 'XML::LibXML::Element' ) {
        my $witstr = $rdg->getAttribute( 'wit' );
        $witstr =~ s/^\s+//;
        $witstr =~ s/\s+$//;
        @wits = split( /\s+/, $witstr );
        map { $_ =~ s/^\#// } @wits;
    }
    return @wits;
}

# Helper with its counters to actually make the readings.
{
    my $word_ctr = 0;
    my %used_nodeids;

    sub save_preexisting_nodeids {
        foreach( @_ ) {
            $used_nodeids{$_->getValue()} = 1;
        }
    }

    sub make_reading {
        my( $graph, $word, $xml_id ) = @_;
        if( $xml_id ) {
            if( exists $used_nodeids{$xml_id} ) {
                if( $used_nodeids{$xml_id} != 1 ) {
                    warn "Already used assigned XML ID somewhere else!";
                    $xml_id = undef;
                }
            } else {
                warn "Undetected pre-existing XML ID";
            }
        }
        if( !$xml_id ) {
            until( $xml_id ) {
                my $try_id = 'w'.$word_ctr++;
                next if exists $used_nodeids{$try_id};
                $xml_id = $try_id;
            }
        }
        my $rdg = $graph->add_reading( $xml_id );
        $rdg->text( $word );
        $used_nodeids{$xml_id} = $rdg;
        return $rdg;
    }
}

1;
