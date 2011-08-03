package Text::Tradition::Parser::CTE;

use strict;
use warnings;
use XML::LibXML;
use XML::LibXML::XPathContext;

=head1 NAME

Text::Tradition::Parser::CTE

=head1 DESCRIPTION

Parser module for Text::Tradition, given a TEI file exported from
Classical Text Editor.

=head1 METHODS

=over

=item B<parse>

my @apparatus = read( $xml_file );

Takes a Tradition object and a TEI file exported from Classical Text
Editor; initializes the Tradition from the file.

=cut

my %seg_readings;  # Save the XML IDs for apparatus anchors.
my %sigil_for;     # Save the XML IDs for witnesses.
my %note_start;    # Save the readings where an apparatus entry is attached.

sub parse {
    my( $tradition, $xml_str ) = @_;
    my $c = $tradition->collation;  # Some shorthand
    
    # First, parse the XML.
    my $parser = XML::LibXML->new();
    my $doc = $parser->parse_string( $xml_str );
    my $tei = $doc->documentElement();
    my $xpc = XML::LibXML::XPathContext->new( $tei );

    # CTE uses a DTD rather than any xmlns-based parsing.  Thus we
    # need no namespace foo.

    # Get the witnesses and create the witness objects.
    foreach my $wit_el ( $xpc->findnodes( '//sourceDesc/listWit/witness' ) ) {
	# The witness xml:id is used internally, and is *not* the sigil name.
	my $id= $wit_el->getAttribute( 'xml:id' );
	$id =~ s/^M/sig/;  # Stupid but there you go.
	my @sig_parts = $xpc->findnodes( './abbr/descendant::text()', $wit_el );
	my $sig = join( '', grep { /\w/ } @sig_parts );
	$tradition->add_witness( sigil => $sig, source => $wit_el->toString() );
	$sigil_for{$id} = $sig;
    }

    # Now go through the text and find the base tokens.  Tokens are
    # either plain text to be split on whitespace, or they are wrapped
    # in <hi/> or <seg/> elements.
    my @base_text;
    my $ctr = 1;
    foreach my $pg_el ( $xpc->findnodes( '/TEI/text/p' ) ) {
	foreach my $xn ( $pg_el->childNodes ) {
	    push( @base_text, _get_readings( $tradition, $xn ) );
	}
    }

    # String together the base.
    my $source = $c->start;
    foreach my $b ( @base_text ) {
	$c->add_path( $source, $b, $c->baselabel );
	$source = $b;
    }
    $c->add_path( $source, $c->add_reading('#END#'), $c->baselabel );
		
    # Now go through the text and find all the apparatus notes, and parse them.
    foreach my $note_el( $xpc->findnodes( '//note[attribute::type = "a1"]' ) ) {
	my $app_start = $note_start{$note_el};
	my $apparatus = _parse_note( $note_el, $c, $app_start );
    }
}


## Recursive little helper function to help us navigate through nested
## XML, picking out the text.

sub _get_readings {
    my( $tradition, $xn ) = @_;
    my @readings;
    if( $xn->nodeType == XML_TEXT_NODE ) {
	my $str = $xn->data;
	$str =~ s/^\s+//;
	foreach my $w ( split( /\s+/, $str ) ) {
	    my $rdg = $tradition->collation->add_reading( 'n'.$ctr++ );
	    $rdg->text( $w );
	    push( @readings, $rdg );
	}
    } elsif( $xn->nodeName eq 'hi' ) {
	foreach( $xn->childNodes ) {
	    # Recurse as if the hi weren't there.
	    push( @readings, _get_readings( $tradition, $_ ) );
	}
    } elsif( $xn->nodeName eq 'seg' ) {
	# Read the reading, but also add the word in question as an anchor.
	my $seg_id = $xn->getAttribute( 'xml:id' );
	my @r;
	foreach( $xn->childNodes ) {
	    push( @r, _get_readings( $tradition, $_ ) );
	}
	warn "More than one reading found in seg $seg_id" unless @r == 1;
	$seg_readings{'#'.$seg_id} = $r[0];
	push( @readings, @r );
    } elsif( $xn->nodeName eq 'note' ) {
	# Save where we found this note.
	$note_start{$xn} = $readings[-1];
    }
    return @readings;
}

## Helper function to parse apparatus entries.  This could get nasty, I mean fun.
sub _parse_note {
    my( $xn, $c, $app_start ) = @_;
    my $app_end = $seg_readings{$xn->getAttribute( 'targetEnd' )};
    my $lemma = join( ' ', map { $_->text } $c->reading_sequence( $app_start, $app_end ) );

    my %seen_wits;
    # TODO A list of active witnesses should be passed really.
    map { $seen_wits{$_} = 0 } vals( %sigil_for );

    # The note has a <p/> tag, then <mentioned/>, then 0-1 text nodes,
    # then an assortment of <hi/> or <abbr/> elements.  If the hi
    # contains an abbr, then it goes before, otherwise it probably
    # goes after.
    my @p = $xn->getChildrenByTagName( 'p' );
    warn "More than one pg in note" unless @p == 1;

    # Strip the <hi/> elements.
    my @childnodes;
    foreach ( $p[0]->childNodes ) {
	if( $_->nodeName eq 'hi' ) {
	    push( @childnodes, $_->childNodes );
	} else {
	    push( @childnodes, $_ );
	}
    }
	
    # Go through and try to parse the sucker.
    my $apparatus;
    my $curr_rdg = '';
    my $reading_sigla = 0;
    my @curr_wits;
    foreach my $pxn ( $p[0]->childNodes ) {
	next if $pxn->nodeName eq 'mentioned';  # Redundant for us.
	if( $pxn->nodeType == XML_TEXT_NODE ) {
	    my $pxn_str = $pxn->data;
	    $pxn_str =~ s/^\s+//;
	    $pxn_str =~ s/\s+$//;
	    my @parts = split( /,\s*/, $pxn_str );
	    if( @parts > 1 ) {
		# Comma separation means that we are starting a new reading.
		my $last = shift @parts;
		if( $last =~ /^\s*a\.\s*c\.\s*$/ ) {
		    my $sig = pop @curr_wits;
		    $sig .= '_ac';
		    push( @curr_wits, $sig );
		}
		$pxn_str = join( ', ', @parts );
		# Trigger a reading interpretation.
		$reading_sigla = 1;
	    }
	    if( $reading_sigla ) {
		my @wits = keys %curr_wits;
		$apparatus->{ interpret( $curr_rdg, $lemma ) } = \@wits;
		$curr_rdg = '';
		$reading_sigla = 0;
		@curr_wits = ();
	    }

	    if( $pxn_str =~ /^\s*a\.\s*c\.\s*$/ ) {
		my $sig = pop @curr_wits;
		$sig .= '_ac';
		push( @curr_wits, $sig );
	    } else {
		$curr_rdg .= $pxn_str;
	    }
	} elsif( $pxn->nodeName eq 'abbr' ) {
	    # It is a witness, stick it in @curr_wits
	    my $wit = $sigil_for{$pxn->getAttribute( 'n' )}
	    push( @curr_wits, $wit ) unless $curr_wits[-1] eq $wit;
	    $seen_wits{$wit} += 1;  # Keep track of a 'seen' count in case there is an a.c.
	    $reading_sigla = 1;
	}
    }
    $apparatus->{ interpret( $curr_rdg, $lemma ) } = \@wits if $curr_rdg;
    $apparatus->{ $lemma } = grep { $seen_wits{$_} == 0 } keys %seen_wits;

    return $apparatus;
}


sub interpret {
    # A utility function to change apparatus-ese into a full variant.
    my( $reading, $lemma ) = @_;
    return $reading if $reading eq $lemma;
    my $oldreading = $reading;
    $lemma =~ s/\s+[[:punct:]]+$//;
    $reading =~ s/\s*\(?sic([\s\w.]+)?\)?$//;
    my @words = split( /\s+/, $lemma );
    if( $reading =~ /^(.*) praem.$/ ) {
	$reading = "$1 $lemma";
    } elsif( $reading =~ /^(.*) add.$/ ) {
        $reading = "$lemma $1";
    } elsif( $reading eq 'om.' ) {
	$reading = '';
    } elsif( $reading eq 'inv.' ) {
        # Hope it is two words.
        print STDERR "WARNING: want to invert a lemma that is not two words\n" 
            unless scalar( @words ) == 2;
        $reading = join( ' ', reverse( @words ) );
    } elsif( $reading eq 'iter.' ) {
        # Repeat the lemma
	$reading = "$lemma $lemma";
     } elsif( $reading =~ /^(.*) \.\.\. (.*)$/ ) {
        # The first and last N words captured should replace the first and
        # last N words of the lemma.
	my @begin = split( /\s+/, $1 );
	my @end = split( /\s+/, $2 );
	if( scalar( @begin ) + scalar ( @end ) > scalar( @words ) ) {
            # Something is wrong and we can't do the splice.
	    print STDERR "ERROR: $lemma is too short to accommodate $oldreading\n";
	} else {
	    splice( @words, 0, scalar @begin, @begin );
	    splice( @words, -(scalar @end), scalar @end, @end );
	    $reading = join( ' ', @words );
	}
    }
    print STDERR "Interpreted $oldreading as $reading given $lemma\n";
    return $reading;
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

