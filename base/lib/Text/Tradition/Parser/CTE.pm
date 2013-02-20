package Text::Tradition::Parser::CTE;

use strict;
use warnings;
use feature 'say';
use Encode qw/ decode /;
use Text::Tradition::Error;
use Text::Tradition::Parser::Util qw/ collate_variants /;
use XML::LibXML;
use XML::LibXML::XPathContext;

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
	
	# First, parse the XML.
    my( $tei, $xpc ) = _remove_formatting( $opts );
    return unless $tei; # we have already warned.

	# CTE uses a DTD rather than any xmlns-based parsing.  Thus we
	# need no namespace handling.
	# Get the witnesses and create the witness objects.
	foreach my $wit_el ( $xpc->findnodes( '//sourceDesc/listWit/witness' ) ) {
		# The witness xml:id is used internally, and is *not* the sigil name.
		my $id= $wit_el->getAttribute( 'xml:id' );
		my @sig_parts = $xpc->findnodes( 'descendant::text()', $wit_el );
		my $sig = _stringify_sigil( @sig_parts );
		say STDERR "Adding witness $sig";
		$tradition->add_witness( sigil => $sig, sourcetype => 'collation' );
		$sigil_for{'#'.$id} = $sig;  # Make life easy by keying on the ID ref syntax
	}
	
	# Now go through the text and find the base tokens, apparatus tags, and
	# anchors.  Make a giant array of all of these things in sequence.
	# TODO consider combining this with creation of graph below
	my @base_text;
	foreach my $pg_el ( $xpc->findnodes( '/TEI/text/body/p' ) ) {
		foreach my $xn ( $pg_el->childNodes ) {
			my @items = _get_base( $xn );
			foreach my $i ( @items ) {
				$DB::single = 1 if $i->{'type'} eq 'anchor' && !$i->{'content'};
			}
			push( @base_text, @items );
		}
	}
	# We now have to work through this array applying the alternate 
	# apparatus readings to the base text.  Essentially we will put 
	# everything on the graph, from which we will delete the apps and
	# anchors when we are done.
	
	# First, put the base tokens, apps, and anchors in the graph.
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
            $apps{$tag} = $item->{'content'};
        }
        $c->add_path( $last, $r, $c->baselabel );
        $last = $r;
    }
    $c->add_path( $last, $c->end, $c->baselabel );
    
    # Now we can parse the apparatus entries, and add the variant readings 
    # to the graph.
    
    foreach my $app_id ( keys %apps ) {
        _add_readings( $c, $app_id );
    }
    
    # Finally, add explicit witness paths, remove the base paths, and remove
    # the app/anchor tags.
    _expand_all_paths( $c );

    # Save the text for each witness so that we can ensure consistency
    # later on
    unless( $opts->{'nocalc'} ) {
		$tradition->collation->text_from_paths();	
		$tradition->collation->calculate_ranks();
		$tradition->collation->flatten_ranks();
	}
}

sub _stringify_sigil {
    my( @nodes ) = @_;
    my @parts = grep { /\w/ } map { $_->data } @nodes;
    my $whole = join( '', @parts );
    $whole =~ s/\W//g;
    return $whole;
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
		push( @readings, map { { 'type' => 'token', 'content' => $_ } } @tokens );
	} elsif( $xn->nodeName eq 'app' ) {
		# Apparatus, just save the entire XML node.
		push( @readings, { 'type' => 'app', 'content' => $xn } );
	} elsif( $xn->nodeName eq 'anchor' ) {
		# Anchor to mark the end of some apparatus; save its ID.
		unless( $xn->getAttribute('type') ) {
			push( @readings, { 'type' => 'anchor', 
			    'content' => $xn->getAttribute( 'xml:id' ) } );
		}
	} elsif ( $xn->nodeName !~ /^(note|seg|milestone|emph)$/ ) {  # Any tag we don't know to disregard
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
    my( $c, $app_id ) = @_;
    my $xn = $apps{$app_id};
    my $anchor = _anchor_name( $xn->getAttribute( 'to' ) );
    # Get the lemma, which is all the readings between app and anchor,
    # excluding other apps or anchors.
    my @lemma = _return_lemma( $c, $app_id, $anchor );
    my $lemma_str = join( ' ', grep { $_ !~ /^__/ } map { $_->text } @lemma );
    
    # For each reading, send its text to 'interpret' along with the lemma,
    # and then save the list of witnesses that these tokens belong to.
    my %wit_rdgs;  # Maps from witnesses to the variant text
    my $ctr = 0;
    my $tag = $app_id;
    $tag =~ s/^\__APP_(.*)\__$/$1/;

    foreach my $rdg ( $xn->getChildrenByTagName( 'rdg' ) ) {
        my @text;
        foreach ( $rdg->childNodes ) {
            push( @text, _get_base( $_ ) );
        }
        my( $interpreted, $flag ) = ( '', undef );
        if( @text ) {
        	( $interpreted, $flag ) = interpret( 
        		join( ' ', map { $_->{'content'} } @text ), $lemma_str );
        }
        next if( $interpreted eq $lemma_str ) && !$flag;  # Reading is lemma.
        
        my @rdg_nodes;
        if( $interpreted eq '#LACUNA#' ) {
        	push( @rdg_nodes, $c->add_reading( { id => 'r'.$tag.".".$ctr++,
        										 is_lacuna => 1 } ) );
        } else {
			foreach my $w ( split( /\s+/, $interpreted ) ) {
				my $r = $c->add_reading( { id => 'r'.$tag.".".$ctr++,
										   text => $w } );
				push( @rdg_nodes, $r );
			}
        }
        # For each listed wit, save the reading.
        foreach my $wit ( split( /\s+/, $rdg->getAttribute( 'wit' ) ) ) {
			$wit .= $flag if $flag;
            $wit_rdgs{$wit} = \@rdg_nodes;
        }
        		
        # Does the reading have an ID? If so it probably has a witDetail
        # attached, and we need to read it.
        if( $rdg->hasAttribute( 'xml:id' ) ) {
        	warn "Witdetail on meta reading" if $flag; # this could get complicated.
            my $rid = $rdg->getAttribute( 'xml:id' );
            my $xpc = XML::LibXML::XPathContext->new( $xn );
            my @details = $xpc->findnodes( './witDetail[@target="'.$rid.'"]' );
            foreach my $d ( @details ) {
                _parse_wit_detail( $d, \%wit_rdgs, \@lemma );
            }
        }
    }       
        
    # Now collate the variant readings, since it is not done for us.
    collate_variants( $c, \@lemma, values %wit_rdgs );
        
    # Now add the witness paths for each reading.
    my $aclabel = $c->ac_label;
    foreach my $wit_id ( keys %wit_rdgs ) {
        my $witstr = _get_sigil( $wit_id, $aclabel );
        my $rdg_list = $wit_rdgs{$wit_id};
        _add_wit_path( $c, $rdg_list, $app_id, $anchor, $witstr );
    }
}

sub _anchor_name {
    my $xmlid = shift;
    $xmlid =~ s/^\#//;
    return sprintf( "__ANCHOR_%s__", $xmlid );
}

sub _return_lemma {
    my( $c, $app, $anchor ) = @_;
    my @nodes = grep { $_->id !~ /^__A(PP|NCHOR)/ } 
        $c->reading_sequence( $c->reading( $app ), $c->reading( $anchor ),
        	$c->baselabel );
    return @nodes;
}

=head2 interpret( $reading, $lemma )

Given a string in $reading and a corresponding lemma in $lemma, interpret what
the actual reading should be. Used to deal with apparatus-ese shorthands for
marking transpositions, prefixed or suffixed words, and the like.

=cut

sub interpret {
	# A utility function to change apparatus-ese into a full variant.
	my( $reading, $lemma ) = @_;
	return $reading if $reading eq $lemma;
	my $oldreading = $reading;
	# $lemma =~ s/\s+[[:punct:]]+$//;
	my $flag;  # In case of p.c. indications
	my @words = split( /\s+/, $lemma );
	if( $reading =~ /^(.*) praem.$/ ) {
		$reading = "$1 $lemma";
	} elsif( $reading =~ /^(.*) add.$/ ) {
		$reading = "$lemma $1";
	} elsif( $reading =~ /add. alia manu/
		|| $reading =~ /inscriptionem compegi e/ # TODO huh?
		|| $reading eq 'inc.'  # TODO huh?
 		) {
		# Ignore it.
		$reading = $lemma;
	} elsif( $reading =~ /locus [uv]acuus/
	    || $reading eq 'def.'
	    || $reading eq 'illeg.'
	    || $reading eq 'onleesbar'
	    ) {
		$reading = '#LACUNA#';
	} elsif( $reading eq 'om.' ) {
		$reading = '';
	} elsif( $reading =~ /^in[uv]\.$/ 
			 || $reading eq 'transp.' ) {
		# Hope it is two words.
		say STDERR "WARNING: want to invert a lemma that is not two words" 
			unless scalar( @words ) == 2;
		$reading = join( ' ', reverse( @words ) );
	} elsif( $reading =~ /^iter(\.|at)$/ ) {
		# Repeat the lemma
		$reading = "$lemma $lemma";
	} elsif( $reading eq 'in marg.' ) {
		# There was nothing before a correction.
		$reading = '';
		$flag = '_ac';
	} elsif( $reading =~ /^(.*?)\s*\(?sic([\s\w.]+)?\)?$/ ) {
		# Discard any 'sic' notation; indeed, indeed.
		$reading = $1;
	} elsif( $reading =~ /^(.*) \.\.\. (.*)$/ ) {
		# The first and last N words captured should replace the first and
		# last N words of the lemma.
		my @begin = split( /\s+/, $1 );
		my @end = split( /\s+/, $2 );
		if( scalar( @begin ) + scalar ( @end ) > scalar( @words ) ) {
			# Something is wrong and we can't do the splice.
			say STDERR "ERROR: $lemma is too short to accommodate $oldreading";
		} else {
			splice( @words, 0, scalar @begin, @begin );
			splice( @words, -(scalar @end), scalar @end, @end );
			$reading = join( ' ', @words );
		}
	}
	if( $oldreading ne $reading || $flag || $oldreading =~ /\./ ) {
		my $int = $reading;
		$int .= " ($flag)" if $flag;
		say STDERR "Interpreted $oldreading as $int given $lemma";
	}
	return( $reading, $flag );
}

sub _parse_wit_detail {
    my( $detail, $readings, $lemma ) = @_;
    my $wit = $detail->getAttribute( 'wit' );
    my $content = $detail->textContent;
    if( $content =~ /a\.\s*c\b/ ) {
        # Replace the key in the $readings hash
        my $rdg = delete $readings->{$wit};
        $readings->{$wit.'_ac'} = $rdg;
        $has_ac{$sigil_for{$wit}} = 1;
    } elsif( $content =~ /p\.\s*c\b/ ) {
        # If no key for the wit a.c. exists, add one pointing to the lemma
        unless( exists $readings->{$wit.'_ac'} ) {
            $readings->{$wit.'_ac'} = $lemma;
        }
        $has_ac{$sigil_for{$wit}} = 1;
    } # else don't bother just yet
}

sub _get_sigil {
    my( $xml_id, $layerlabel ) = @_;
    if( $xml_id =~ /^(.*)_ac$/ ) {
        my $real_id = $1;
        return $sigil_for{$real_id} . $layerlabel;
    } else {
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
    foreach my $v ( $c->sequence->isolated_vertices ) {
    	my $r = $c->reading( $v );
    	say STDERR "Deleting orphan reading $r / " . $r->text;
    	$c->del_reading( $r );
    }
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

sub throw {
	Text::Tradition::Error->throw( 
		'ident' => 'Parser::CTE error',
		'message' => $_[0],
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

