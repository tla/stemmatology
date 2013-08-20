package Text::Tradition::Parser::CTE;

use strict;
use warnings;
use feature 'say';
use Encode qw/ decode /;
use Text::Tradition::Error;
use Text::Tradition::Parser::Util qw/ collate_variants /;
use XML::LibXML;
use XML::LibXML::XPathContext;
use TryCatch;

=head1 NAME

Text::Tradition::Parser::CTE

=head1 DESCRIPTION

Parser module for Text::Tradition, given a TEI file exported from
Classical Text Editor.

=head1 METHODS

=head2 parse

my @apparatus = read( $xml_file );

Takes a Tradition object and a TEI file exported from Classical Text
Editor using double-endpoint-attachment critical apparatus encoding; 
initializes the Tradition from the file.

=cut

my %sigil_for;  # Save the XML IDs for witnesses.
my %apps;       # Save the apparatus XML for a given ID.    
my %has_ac;     # Keep track of witnesses that have corrections.

sub parse {
	my( $tradition, $opts ) = @_;
	my $c = $tradition->collation;	# Some shorthand
	
	## DEBUG/TEST
	$opts->{interpret_transposition} = 1;
	
	# First, parse the XML.
    my( $tei, $xpc ) = _remove_formatting( $opts );
    return unless $tei; # we have already warned.

	# CTE uses a DTD rather than any xmlns-based parsing.  Thus we
	# need no namespace handling.
	# Get the witnesses and create the witness objects.
	foreach my $wit_el ( $xpc->findnodes( '//sourceDesc/listWit/witness' ) ) {
		# The witness xml:id is used internally, and is *not* the sigil name.
		my $id= $wit_el->getAttribute( 'xml:id' );
		# If the witness element has an abbr element, that is the sigil. Otherwise
		# the whole thing is the sigil.
		my $sig = $xpc->findvalue( 'abbr', $wit_el );
		my $identifier = 'CTE witness';
		if( $sig ) {
			# The sigil is what is in the <abbr/> tag; the identifier is anything
			# that follows. 
			$identifier = _tidy_identifier( 
				$xpc->findvalue( 'child::text()', $wit_el ) );
		} else {
			my @sig_parts = $xpc->findnodes( 'descendant::text()', $wit_el );
			$sig = _stringify_sigil( @sig_parts );
		}
		say STDERR "Adding witness $sig ($identifier)";
		$tradition->add_witness( sigil => $sig, identifier => $identifier, 
			sourcetype => 'collation' );
		$sigil_for{'#'.$id} = $sig;  # Make life easy by keying on the ID ref syntax
	}
	
	# Now go through the text and find the base tokens, apparatus tags, and
	# anchors.  Make a giant array of all of these things in sequence.
	# TODO consider combining this with creation of graph below
	my @base_text;
	foreach my $pg_el ( $xpc->findnodes( '/TEI/text/body/p' ) ) {
		foreach my $xn ( $pg_el->childNodes ) {
			push( @base_text, _get_base( $xn ) );
		}
	}
	# We now have to work through this array applying the alternate 
	# apparatus readings to the base text.  Essentially we will put 
	# everything on the graph, from which we will delete the apps and
	# anchors when we are done.
	
	# First, put the base tokens, apps, and anchors in the graph. Save the
	# app siglorum separately as it has to be processed in order.
	my @app_sig;
	my @app_crit;
	my $counter = 0;
	my $last = $c->start;
	foreach my $item ( @base_text ) {
	    my $r;
        if( $item->{'type'} eq 'token' ) {
            $r = $c->add_reading( { id => 'n'.$counter++, 
            						text => $item->{'content'} } );
        } elsif ( $item->{'type'} eq 'anchor' ) {
            $r = $c->add_reading( { id => '__ANCHOR_' . $item->{'content'} . '__', 
            						is_ph => 1 } );
        } elsif ( $item->{'type'} eq 'app' ) {
            my $tag = '__APP_' . $counter++ . '__';
            $r = $c->add_reading( { id => $tag, is_ph => 1 } );
            my $app = $item->{'content'};
            $apps{$tag} = $app;
            # Apparatus should be differentiable by type attribute; apparently
            # it is not. Peek at the content to categorize it.
            # Apparatus criticus is type a1; app siglorum is type a2
            my @sigtags = $xpc->findnodes( 
            	'descendant::*[name(witStart) or name(witEnd)]', $app );
            if( @sigtags ) {
	        	push( @app_sig, $tag );
	        } else {
	            push( @app_crit, $tag );
	        }
        }
        $c->add_path( $last, $r, $c->baselabel );
        $last = $r;
    }
    $c->add_path( $last, $c->end, $c->baselabel );
    
    # Now we can parse the apparatus entries, and add the variant readings 
    # to the graph.
    foreach my $app_id ( @app_crit ) {
        _add_readings( $c, $app_id, $opts );
    }
    _add_lacunae( $c, $opts, @app_sig );
    
    # Finally, add explicit witness paths, remove the base paths, and remove
    # the app/anchor tags.
    try {
	    _expand_all_paths( $c );
	} catch( Text::Tradition::Error $e ) {
		throw( $e->message );
	} catch {
		throw( $@ );
	}

    # Save the text for each witness so that we can ensure consistency
    # later on
    unless( $opts->{'nocalc'} ) {
    	try {
			$tradition->collation->text_from_paths();	
			$tradition->collation->calculate_ranks();
			$tradition->collation->flatten_ranks();
		} catch( Text::Tradition::Error $e ) {
			throw( $e->message );
		} catch {
			throw( $@ );
		}
	}
}

sub _stringify_sigil {
    my( @nodes ) = @_;
    my @parts = grep { /\w/ } map { $_->data } @nodes;
    my $whole = join( '', @parts );
    $whole =~ s/\W//g;
    return $whole;
}

sub _tidy_identifier {
	my( $str ) = @_;
	$str =~ s/^\W+//;
	return $str;
}

# Get rid of all the formatting elements that get in the way of tokenization.
sub _remove_formatting {
	my( $opts ) = @_;
	
	# First, parse the original XML
	my $parser = XML::LibXML->new();
    my $doc;
    if( exists $opts->{'string'} ) {
        $doc = $parser->parse_string( $opts->{'string'} );
    } elsif ( exists $opts->{'file'} ) {
        $doc = $parser->parse_file( $opts->{'file'} );
    } elsif ( exists $opts->{'xmlobj'} ) {
    	$doc = $opts->{'xmlobj'};
    } else {
        warn "Could not find string or file option to parse";
        return;
    }

    # Second, remove the formatting
	my $xpc = XML::LibXML::XPathContext->new( $doc->documentElement );
	my @useless = $xpc->findnodes( '//hi' );
	foreach my $n ( @useless ) {
		my $parent = $n->parentNode();
		my @children = $n->childNodes();
		my $first = shift @children;
		if( $first ) {
			$parent->replaceChild( $first, $n );
			foreach my $c ( @children ) {
				$parent->insertAfter( $c, $first );
				$first = $c;
			}
		} else {
			$parent->removeChild( $n );
		}
	}
	
	# Third, write out and reparse to merge the text nodes.
	my $enc = $doc->encoding || 'UTF-8';
	my $result = decode( $enc, $doc->toString() );
	my $tei = $parser->parse_string( $result )->documentElement;
	unless( $tei->nodeName =~ /^tei(corpus)?$/i ) {
		throw( "Parsed document has non-TEI root element " . $tei->nodeName );
	}
	$xpc = XML::LibXML::XPathContext->new( $tei );
	return( $tei, $xpc );
}

## Helper function to help us navigate through nested XML, picking out 
## the words, the apparatus, and the anchors.

sub _get_base {
	my( $xn ) = @_;
	my @readings;
	if( $xn->nodeType == XML_TEXT_NODE ) {
	    # Base text, just split the words on whitespace and add them 
	    # to our sequence.
		my $str = $xn->data;
		$str =~ s/^\s+//;
		my @tokens = split( /\s+/, $str );
		push( @readings, map { { type => 'token', content => $_ } } @tokens );
	} elsif( $xn->nodeName eq 'app' ) {
		# Apparatus, just save the entire XML node.
		push( @readings, { type => 'app', content => $xn } );
	} elsif( $xn->nodeName eq 'anchor' ) {
		# Anchor to mark the end of some apparatus; save its ID.
		if( $xn->hasAttribute('xml:id') ) {
			push( @readings, { type => 'anchor', 
			    content => $xn->getAttribute( 'xml:id' ) } );
		} # if the anchor has no XML ID, it is not relevant to us.
	} elsif( $xn->nodeName !~ /^(note|seg|milestone|emph|witStart|witEnd)$/ ) {  
		# Any tag we don't know to disregard
	    say STDERR "Unrecognized tag " . $xn->nodeName;
	}
	return @readings;
}

sub _append_tokens {
	my( $list, @tokens ) = @_;
	if( @$list && $list->[-1]->{'content'} =~ /\#JOIN\#$/ ) {
		# The list evidently ended mid-word; join the next token onto it.
		my $t = shift @tokens;
		if( ref $t && $t->{'type'} eq 'token' ) {
			# Join the word
			$t = $t->{'content'};
		} elsif( ref $t ) {
			# An app or anchor intervened; end the word.
			unshift( @tokens, $t );
			$t = '';
		}
		$list->[-1]->{'content'} =~ s/\#JOIN\#$/$t/;
	}
	foreach my $t ( @tokens ) {
		unless( ref( $t ) ) {
			$t = { 'type' => 'token', 'content' => $t };
		}
		push( @$list, $t );
	}
}

sub _add_readings {
    my( $c, $app_id, $opts ) = @_;
    my $xn = $apps{$app_id};
    my $anchor = _anchor_name( $xn->getAttribute( 'to' ) );
    
    # For each reading, send its text to 'interpret' along with the lemma,
    # and then save the list of witnesses that these tokens belong to.
    my %wit_rdgs;  # Maps from witnesses to the variant text
    my $ctr = 0;
    my $tag = $app_id;
    $tag =~ s/^\__APP_(.*)\__$/$1/;

    foreach my $rdg ( $xn->getChildrenByTagName( 'rdg' ) ) {
    	# Get the relevant witnesses.
    	my @witlist = split( /\s+/, $rdg->getAttribute( 'wit' ) );

        # Does the reading have an ID? If so it probably has a witDetail
        # attached, and we need to read it. If an A.C. or P.C. reading is
        # declared explicity, this is where it will be dealt with.
        if( $rdg->hasAttribute( 'xml:id' ) ) {
            my $rid = $rdg->getAttribute( 'xml:id' );
            my $xpc = XML::LibXML::XPathContext->new( $xn );
            my @details = $xpc->findnodes( './witDetail[@target="'.$rid.'"]' );
            foreach my $d ( @details ) {
                @witlist = _parse_wit_detail( $d, @witlist );
            }
        }

		# Now we have our list of relevant witnesses for the reading, annotated
		# with AC or PC if applicable. Interpret the reading in light of the 
		# lemma, anything we already have for the witness, etc.
        # If an A.C. or P.C. reading is implied rather than explicitly noted,
        # this is where it will be dealt with.
        
		foreach my $wit ( @witlist ) {
			# First get the lemma for this witness. This is all the readings
			# between app and anchor, excluding other apps or anchors.
			my @testwits;
			my $sigil;
			my $acsigil;
			if( $wit =~ /^(.*)_pc$/ ) {
				# If this is a p.c., it is the 'main' witness and we need to
				# track the a.c. version separately.
				$sigil = _get_sigil( $1 );
				$acsigil = $sigil . $c->ac_label;
			} elsif ( $wit =~ /^(.*)_ac$/ ) {
				# If this is an a.c., we use the main witness as backup in our
				# lemma query.
				my $basesigil = _get_sigil( $1 );
				$sigil = $basesigil . $c->ac_label;
				@testwits = ( $sigil, $basesigil );
			}
			@testwits = ( $sigil ) unless @testwits;
			
			my @lemma = _return_lemma( $c, $app_id, $anchor, @testwits );
			my @aclemma;
			if( $acsigil ) {
				@aclemma = _return_lemma( $c, $app_id, $anchor, 
					$acsigil, $testwits[0] ); # @testwits contains the sigil
			}
			
			# Now remove the witness path temporarily - we will restore it
			# after interpreting the reading.
			my $from = $app_id;
			foreach my $to ( ( @lemma, $anchor ) ) {
				last if $to eq $anchor;
				$c->del_path( $from, $to, $sigil );
				$from = $to;
			}
			if( $acsigil ) {
				# Do the same for the aclemma.
				$from = $app_id;
				foreach my $to ( ( @aclemma, $anchor ) ) {
					last if $to eq $anchor;
					$c->del_path( $from, $to, $acsigil );
					$from = $to;
				}
        	}
        	
			my @rdg_nodes = _read_reading( $c, $rdg, $wit, \@lemma, \@aclemma, 
				$tag, \$ctr, $anchor, $opts );
			$wit_rdgs{$wit} = \@rdg_nodes;
			# If we now have a new lemma for a.c., set it.
			if( @aclemma ) {
				$wit_rdgs{$wit.'_ac'} = \@aclemma;
			}
        }		
    }       
    
    my @baselemma = _return_lemma( $c, $app_id, $anchor );
    # Now collate the variant readings, since it is not done for us.
    collate_variants( $c, \@baselemma, values %wit_rdgs );
        
    # Now add the witness paths for each reading.
	foreach my $wit_id ( keys %wit_rdgs ) {
		my $sigil = _get_sigil( $wit_id, $c->ac_label );
		my $rdg_list = $wit_rdgs{$wit_id};
		_add_wit_path( $c, $rdg_list, $app_id, $anchor, $sigil );
	}
}

sub _anchor_name {
    my $xmlid = shift;
    $xmlid =~ s/^\#//;
    return sprintf( "__ANCHOR_%s__", $xmlid );
}

# Return the reading sequence for the specified witness (and backup, if
# applicable.) If no witness sigla are specified, use the base sequence.
sub _return_lemma {
    my( $c, $app, $anchor, @sigla ) = @_;
    push( @sigla, $c->baselabel );
    my @nodes = grep { $_->id !~ /^__A(PP|NCHOR)/ } 
        $c->reading_sequence( $c->reading( $app ), $c->reading( $anchor ), @sigla );
    return @nodes;
}

# Look at the witDetail and modify any affected witnesses. For example,
# an a.c. annotation in the detail applied to witness #M206 will change
# the list ( #M130, #M54, #M206 ) to ( #M130, #M54, #M206_ac ). Preserve
# ordering.
sub _parse_wit_detail {
	my( $detail, @wits ) = @_;
    my %witmap;
    map { $witmap{$_} = $_ } @wits;
    my @changewits = split( /\s+/, $detail->getAttribute( 'wit' ) );
    my $content = $detail->textContent;
    if( $content =~ /^a\.?\s*c(orr)?\.$/ ) {
        # The witness in question is actually an a.c. witness
        map { $witmap{$_} = $_.'_ac' } @changewits;
    } elsif( $content =~ /^p\.?\s*c(orr)?\.$/ || $content =~ /^s\.?\s*l\.$/
    	|| $content =~ /^in marg\.?$/ ) {
        # The witness in question is actually a p.c. witness
        map { $witmap{$_} = $_.'_pc' } @changewits;
    } else {  #...not sure what it is?
    	say STDERR "WARNING: Unrecognized sigil annotation $content";
    }
    return map { $witmap{$_} } @wits;
}

sub _read_reading {
	my( $c, $rdg, $witness, $lemma, $aclemma, $tag, $ctr, $anchor, $opts ) = @_;

	# Get the text of the lemma.	
	my $lemma_str = join( ' ',  map { $_->text } grep { !$_->is_ph } @$lemma );

	my @text;
	foreach ( $rdg->childNodes ) {
		push( @text, _get_base( $_ ) );
	}
	my( $interpreted, $flag ) = ( '', undef );
	if( @text ) {
		( $interpreted, $flag ) = interpret( 
			join( ' ', map { $_->{'content'} } @text ), $lemma_str, $anchor, $opts );
	}
	if( ( $interpreted eq $lemma_str || $interpreted eq '__LEMMA__' ) 
		&& !keys %$flag ) {
		# The reading is the lemma. Pass it back.
		return @$lemma;
	}
	
	my @rdg_nodes;
	if( $interpreted eq '#LACUNA#' ) {
		push( @rdg_nodes, $c->add_reading( { id => 'r'.$tag.".".$$ctr++,
											 is_lacuna => 1 } ) );
	} elsif( $flag->{'TR'} ) {
		# Our reading is transposed to after the given string. Look
		# down the collation base text and try to find it.
		# The @rdg_nodes should remain blank here, so that the correct
		# omission goes into the graph.
		my @transp_nodes;
		foreach my $w ( split(  /\s+/, $interpreted ) ) {
			my $r = $c->add_reading( { id => 'r'.$tag.".".$$ctr++,
									   text => $w } );
			push( @transp_nodes, $r );
		}
		if( $anchor && $lemma ) {
			my $aname = _anchor_name( $anchor );
			my $success = _attach_transposition( $c, $lemma, $aname, 
				\@transp_nodes, $witness, $flag->{'TR'} );
			unless( $success ) {
				# If we didn't manage to insert the displaced reading,
				# then restore it here rather than silently deleting it.
				push( @rdg_nodes, @transp_nodes );
			}
		}
	} else {
		# Create the reading nodes.
		# First figure out whether we are making an a.c. lemma, p.c. lemma,
		# or main lemma, and adjust the list accordingly.
		my $use_list = \@rdg_nodes;
		if( $flag->{'AC'} ) {
			# First check that we are not doubling up a.c. and p.c. designations
			if( @$aclemma ) {
				throw( "Cannot have a.c. designation in text on p.c. witness "
						. "at $tag -> $anchor" );
			} elsif( $witness =~ /_ac$/ ) {
				throw( "Cannot have p.c. designation in text on a.c. witness "
						. "at $tag -> $anchor" );
			}
			# Stick the interpreted reading into aclemma, and return the original
			# lemma for the main witness.
			$use_list = $aclemma;
			push( @rdg_nodes, @$lemma );
		} elsif( $flag->{'PC'} ) {
			# First check that we are not doubling up a.c. and p.c. designations
			if( @$aclemma ) {
				throw( "Cannot have p.c. designation in text on p.c. witness "
						. "at $tag -> $anchor" );
			} elsif( $witness =~ /_ac$/ ) {
				throw( "Cannot have p.c. designation in text on a.c. witness "
						. "at $tag -> $anchor" );
			}
			# Stick the original lemma into aclemma, and return our interpretation
			# for the main witness.			
			@$aclemma = @$lemma;
		}
		
		# Fill out the reading we will return.
		foreach my $w ( split( /\s+/, $interpreted ) ) {
			if( $w eq '__LEMMA__' ) {
				push( @$use_list, @$lemma );
			} else {
				my $r = $c->add_reading( { id => 'r'.$tag.".".$$ctr++,
										   text => $w } );
				push( @$use_list, $r );
			}
		}
	}
	
	return @rdg_nodes;
}

=head2 interpret( $reading, $lemma )

Given a string in $reading and a corresponding lemma in $lemma, interpret what
the actual reading should be. Used to deal with apparatus-ese shorthands for
marking transpositions, prefixed or suffixed words, and the like.

=cut

sub interpret {
	# A utility function to change apparatus-ese into a full variant.
	my( $reading, $lemma, $anchor, $opts ) = @_;
	return $reading if $reading eq $lemma;
	my $oldreading = $reading;
	# $lemma =~ s/\s+[[:punct:]]+$//;
	my $flag = {};  # To pass back extra info about the interpretation
	my @words = split( /\s+/, $lemma );
	# Discard any 'sic' notation - that rather goes without saying.
	$reading =~ s/([[:punct:]]+)?sic([[:punct:]]+)?//g;
	
	# Look to see if there is an implied add or praem masked by the XML.
	# If so, undo it for purposes of reading identity.
	$reading =~ s/^$lemma\b/__LEMMA__/;
	$reading =~ s/\b$lemma$/__LEMMA__/;
	
	# Now look for common jargon.
	if( $reading =~ /^(.*) praem.$/ || $reading =~ /^praem\. (.*)$/ ) {
		$reading = "$1 __LEMMA__";
	} elsif( $reading =~ /^(.*) add.$/ || $reading =~ /^add\. (.*)$/ ) {
		$reading = "__LEMMA__ $1";
	} elsif( $reading =~ /locus [uv]acuus/
	    || $reading eq 'def.'
	    || $reading eq 'illeg.'
	    || $reading eq 'desunt'
	    ) {
		$reading = '#LACUNA#';
	} elsif( $reading eq 'om.' ) {
		$reading = '';
	} elsif( $reading =~ /^in[uv]\.$/ 
			 || $reading =~ /^tr(ans(p)?)?\.$/ ) {
		# Hope it is two words.
		say STDERR "WARNING: want to invert a lemma that is not two words" 
			unless scalar( @words ) == 2;
		$reading = join( ' ', reverse( @words ) );
	} elsif( $reading =~ /^iter(\.|at)$/ ) {
		# Repeat the lemma
		$reading = "__LEMMA__ $lemma";
	} elsif( $reading =~ /^(.*?)\s*\(?in marg\.\)?$/ ) {
		$reading = $1;
		if( $reading ) {
			# The given text is a correction.
			$flag->{'PC'} = 1;
		} else {
			# The lemma itself was the correction; the witness carried
			# no reading pre-correction.
			$flag->{'AC'} = 1;
		}
	} elsif( $reading =~ /^(.*) \.\.\. (.*)$/ ) {
		# The first and last N words captured should replace the first and
		# last N words of the lemma.
		my @begin = split( /\s+/, $1 );
		my @end = split( /\s+/, $2 );
		if( scalar( @begin ) + scalar ( @end ) > scalar( @words ) ) {
			# Something is wrong and we can't do the splice.
			throw( "$lemma is too short to accommodate $oldreading" );
		} else {
			splice( @words, 0, scalar @begin, @begin );
			splice( @words, -(scalar @end), scalar @end, @end );
			$reading = join( ' ', @words );
		}
	} elsif( $opts->{interpret_transposition} &&
			 ( $reading =~ /^post\s*(?<lem>.*?)\s+tr(ans(p)?)?\.$/ || 
			   $reading =~ /^tr(ans(p)?)?\. post\s*(?<lem>.*)$/) ) {
		# Try to deal with transposed readings
		## DEBUG
		say STDERR "Will attempt transposition: $reading at $anchor";
		# Copy the lemma into the reading string for insertion later
		# in the text.
		$reading = $lemma;
		$flag->{'TR'} = $+{lem};
	}
	return( $reading, $flag );
}

# Make a best-effort attempt to attach a transposition farther down the line.
# $lemmaseq contains the Reading objects of the lemma
# $anchor contains the point at which we should start scanning for a match
# $rdgseq contains the Reading objects of the transposed reading 
# 	(should be identical to the lemma)
# $witness contains the applicable witness
# $reftxt contains the text to match, after which the $rdgseq should go.
sub _attach_transposition {
	my( $c, $lemmaseq, $anchor, $rdgseq, $witness, $reftxt ) = @_;
	my @refwords = split( /\s+/, $reftxt );
	my $checked = $c->reading( $anchor );
	my $found;
	my $success;
	while( $checked ne $c->end && !$found ) {
		my $next = $c->next_reading( $checked, $c->baselabel );
		if( $next->text eq $refwords[0] ) {
			# See if the entire sequence of words matches.
			$found = $next;
			foreach my $w ( 1..$#refwords ) {
				$found = $c->next_reading( $next, $c->baselabel );
				unless( $found->text eq $refwords[$w] ) {
					$found = undef;
					last;
				}
			}
		}
		$checked = $next;
	}
	if( $found ) {
		# The $found variable should now contain the reading after which we
		# should stick the transposition.
		my $fnext = $c->next_reading( $found, $c->baselabel );
		my $sigil = _get_sigil( $witness, $c->ac_label );
		_add_wit_path( $c, $rdgseq, $found->id, $fnext->id, $sigil );
		# ...and add the transposition relationship between lemma and rdgseq.
		if( @$lemmaseq == @$rdgseq ) {
			foreach my $i ( 0..$#{$lemmaseq} ) {
				$c->add_relationship( $lemmaseq->[$i], $rdgseq->[$i],
					{ type => 'transposition', annotation => 'Detected by CTE' } );
			}
		$success = 1;
		} else {
			throw( "Lemma at $found and transposed sequence different lengths?!" );
		}
	} else {
		say STDERR "WARNING: Unable to find $reftxt in base text for transposition";
	}
	return $success;
}

sub _add_lacunae {
	my( $c, $opts, @app_ids ) = @_;
	# Go through the apparatus entries in order, noting where to start and stop our
	# various witnesses.
	my %lacunose;
	foreach my $app_id ( @app_ids ) {
		my $app = $apps{$app_id};
		my $ctr = 0;
		# Find the anchor, if any. 
		my $anchor = $app->getAttribute( 'to' );
		next unless $anchor; # Skip any app without an anchor.
							 # It is probably the initial witStart.
		my $aname;
		$anchor =~ s/^\#//;
		$aname = _anchor_name( $anchor );

		foreach my $rdg ( $app->getChildrenByTagName( 'rdg' ) ) {
			# Get the affected witnesses. We are not parsing any witDetail right
			# now so none of these will be a.c. or p.c. etc.
    		my @witlist = split( /\s+/, $rdg->getAttribute( 'wit' ) );
			my @start = $rdg->getChildrenByTagName( 'witStart' );
			my @end = $rdg->getChildrenByTagName( 'witEnd' );
			if( @start && @end ) {
				throw( "App sig entry at $anchor has both witStart and witEnd!" );
			}
			# Parse the reading itself
			my $lacunanode;
			foreach my $wit ( @witlist ) {
				my $aclemma = []; # Should stay unused!!
				my $tag = $app_id;
				$tag =~ s/__APP_(.*)__$/$1/;
				my $sigil = _get_sigil( $wit );
				$DB::single = 1 if $app_id eq '__APP_1999__' && $aname eq '__ANCHOR_w1577__';
				my @lemma = _return_lemma( $c, $app_id, $aname, $sigil );
				my @rdg_nodes = _read_reading( $c, $rdg, $wit, \@lemma, $aclemma, 
					$tag, \$ctr, $anchor, $opts );

				if( @$aclemma ) {
					throw( "Cannot have a.c. or p.c. notation where a witness starts "
							. "or ends at $tag -> $anchor" );
				}
				if( @start && 
					$c->prior_reading( $aname, $c->baselabel ) ne $c->start ) {
					# We are picking back up after a hiatus. Find the last end and
					# add a lacuna link between there and here.
					my $stoppoint = delete $lacunose{$sigil};
					my $stopname = $stoppoint ? _anchor_name( $stoppoint ) : $c->start->id;
					say STDERR "Adding lacuna for $sigil between $stopname and $anchor";
					unless( $lacunanode ) {
						$lacunanode = $c->add_reading( 
							{ id => "as_$tag"."_$anchor".$ctr++, is_lacuna => 1 } );
					}
					unshift( @rdg_nodes, $lacunanode );
        			_add_wit_path( $c, \@rdg_nodes, $stopname, $aname, $sigil );
				} elsif( @end && 
					$c->next_reading( $aname, $c->baselabel ) ne $c->end ) {
					# We are stopping. If we've already stopped for the given witness,
					# flag an error; otherwise record the stopping point.
					if( $lacunose{$sigil} ) {
						throw( "Trying to end $sigil at $anchor when already ended at "
							. $lacunose{$sigil} );
					}
					# Add in the interpreted reading, whatever it was.
					_add_wit_path( $c, \@rdg_nodes, $app_id, $aname, $sigil );					
					$lacunose{$sigil} = $anchor;
				}
			}
		}
	}
	
	# For whatever remains in the %lacunose hash, add a lacuna between that spot and
	# $c->end for each of the witnesses.
	my $ctr = 0;
	foreach my $sigil ( keys %lacunose ) {
		next unless $lacunose{$sigil};
		my $anchor = $lacunose{$sigil};
		my $aname = _anchor_name( $anchor );
		say STDERR "Adding lacuna for $sigil from $aname to end";
		my $lacuna = $c->add_reading( { id => "as_end_$anchor.".$ctr++,
			is_lacuna => 1 } );
		_add_wit_path( $c, [ $lacuna ], $aname, $c->end, $sigil );
	}
}

# Utility function to take an XML ID, e.g. #M206, and return the actual
# sigil, e.g. Q. If _ac is part of the XML ID then it will be replaced
# with the contents of $layerlabel.
sub _get_sigil {
    my( $xml_id, $layerlabel ) = @_;
    if( $xml_id =~ /^(.*)_ac$/ ) {
        my $real_id = $1;
        throw( "Tried to get a sigil for a layered witness with no layerlabel" )
        	unless $layerlabel;
        throw( "No sigil defined for $real_id" ) unless exists $sigil_for{$real_id};
        return $sigil_for{$real_id} . $layerlabel;
    } else {
        throw( "No sigil defined for $xml_id" ) unless exists $sigil_for{$xml_id};
        return $sigil_for{$xml_id};
    }
}

sub _expand_all_paths { 
    my( $c ) = @_;
    
    # Walk the collation and fish out the paths for each witness
    foreach my $wit ( $c->tradition->witnesses ) {
        my $sig = $wit->sigil;
        my @path = grep { !$_->is_ph } 
            $c->reading_sequence( $c->start, $c->end, $sig );
        $wit->path( \@path );
        if( $has_ac{$sig} ) {
            my @ac_path = grep { !$_->is_ph } 
                $c->reading_sequence( $c->start, $c->end, $sig.$c->ac_label );
            $wit->uncorrected_path( \@ac_path );
        }
    }   
    
    # Delete the anchors
    foreach my $anchor ( grep { $_->is_ph } $c->readings ) {
        $c->del_reading( $anchor );
    }
    # Delete the base edges
    map { $c->del_path( $_, $c->baselabel ) } $c->paths;
    
    # Make the path edges
    $c->make_witness_paths();
    
    # Now remove any orphan nodes, and warn that we are doing so.
    my %suspect_apps;
    while( $c->sequence->predecessorless_vertices > 1 ) {
    	foreach my $v ( $c->sequence->predecessorless_vertices ) {
	    	my $r = $c->reading( $v );
	    	next if $r->is_start;
	    	my $tag = $r->id;
	    	$tag =~ s/^r(\d+)\.\d+/$1/;
    		say STDERR "Deleting orphan reading $r / " . $r->text;
    		push( @{$suspect_apps{$tag}}, $r->id ) if $tag =~ /^\d+$/;
    		$c->del_reading( $r );
    	}
    }
    if( $c->sequence->successorless_vertices > 1 ) {
    	my @bad = grep { $_ ne $c->end->id } $c->sequence->successorless_vertices;
    	foreach( @bad ) {
    		my $tag = $_;
    		next unless $tag =~ /^r/;
    		$tag =~ s/^r(\d+)\.\d+/$1/;
    		push( @{$suspect_apps{$tag}}, $_ );
    	}
		_dump_suspects( %suspect_apps );
    	throw( "Remaining hanging readings: @bad" );
	}
	_dump_suspects( %suspect_apps ) if keys %suspect_apps;
}

sub _add_wit_path {
    my( $c, $rdg, $app, $anchor, $wit ) = @_;
    my @nodes = @$rdg;
    push( @nodes, $c->reading( $anchor ) );
    
    my $cur = $c->reading( $app );
    foreach my $n ( @nodes ) {
        $c->add_path( $cur, $n, $wit );
        $cur = $n;
    }
}

sub _dump_suspects {
	my %list = @_;
	say STDERR "Suspect apparatus entries:";
	foreach my $suspect ( sort { $a <=> $b } keys %list ) {
		my @badrdgs = @{$list{$suspect}};
		say STDERR print_apparatus( $suspect );
		say STDERR "\t(Linked to readings @badrdgs)";
	}
}

sub print_apparatus {
	my( $appid ) = @_;
	my $tag = '__APP_' . $appid . '__';
	my $app = $apps{$tag};
	my $appstring = '';
	# Interpret the XML - get the lemma and readings and print them out.
	my $xpc = XML::LibXML::XPathContext->new( $app );
	my $anchor = $app->getAttribute('to');
	if( $anchor ) {
		# We have a lemma, so we construct it.
		$anchor =~ s/^#//;
		$appstring .= "(Anchor $anchor) ";
		my $curr = $app;
		while( $curr ) {
			last if $curr->nodeType eq XML_ELEMENT_NODE 
				&& $curr->hasAttribute( 'xml:id' ) 
				&& $curr->getAttribute( 'xml:id' ) eq $anchor;
			$appstring .= $curr->data if $curr->nodeType eq XML_TEXT_NODE;
			$curr = $curr->nextSibling;
		}
	}
	$appstring .= '] ';
	my @readings;
	foreach my $rdg_el ( $xpc->findnodes( 'child::rdg' ) ) {
		my $rdgtext = '';
		my $startend = '';
		my %detail;
		foreach my $child_el ( $rdg_el->childNodes ) {
			if( $child_el->nodeType eq XML_TEXT_NODE ) {
				$rdgtext .= $child_el->data;
			} elsif( $child_el->nodeName =~ /^wit(Start|End)$/ ) {
				my $startend = lc( $1 );
			} elsif( $child_el->nodeName eq 'witDetail' ) {
				foreach my $wit ( map { _get_sigil( $_ ) } 
					split( /\s+/, $child_el->getAttribute('wit') ) ) {
					$detail{$wit} = $child_el->textContent;
				}
			}
		}
		
		my @witlist;
		foreach my $witrep (  map { _get_sigil( $_ ) } 
			split( /\s+/, $rdg_el->getAttribute('wit') ) ) {
			if( exists $detail{$witrep} ) {
				$witrep .= '(' . $detail{$witrep} . ')'
			}
			if( $startend eq 'start' ) {
				$witrep = '*' . $witrep;
			} elsif( $startend eq 'end' ) {
				$witrep .= '*';
			}
			push( @witlist, $witrep );
		}
		$rdgtext .= " @witlist";
		push( @readings, $rdgtext );
	}
	$appstring .= join( '  ', @readings );
	return $appstring;
}

sub throw {
	my( $message, $app ) = @_;
	if( $app ) {
		$message = "$message\nApparatus entry:\n" . print_apparatus( $app );
	}
	Text::Tradition::Error->throw( 
		'ident' => 'Parser::CTE error',
		'message' => $message,
		);
}

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews, aurum@cpan.org

=cut

1;

