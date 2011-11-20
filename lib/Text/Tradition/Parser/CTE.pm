package Text::Tradition::Parser::CTE;

use strict;
use warnings;
use Text::Tradition::Parser::Util qw/ collate_variants /;
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
	my $tei = $doc->documentElement();
	my $xpc = XML::LibXML::XPathContext->new( $tei );

	# CTE uses a DTD rather than any xmlns-based parsing.  Thus we
	# need no namespace handling.

	# Get the witnesses and create the witness objects.
	foreach my $wit_el ( $xpc->findnodes( '//sourceDesc/listWit/witness' ) ) {
		# The witness xml:id is used internally, and is *not* the sigil name.
		my $id= $wit_el->getAttribute( 'xml:id' );
		my @sig_parts = $xpc->findnodes( './abbr/descendant::text()', $wit_el );
		my $sig = _stringify_sigil( @sig_parts );
		$tradition->add_witness( sigil => $sig, source => $wit_el->toString() );
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
	
	# First, put the base tokens, apps, and anchors in the graph.
	my $counter = 0;
	my $last = $c->start;
	foreach my $item ( @base_text ) {
	    my $r;
        if( $item->{'type'} eq 'token' ) {
            $r = $c->add_reading( 'n'.$counter++ );
            $r->text( $item->{'content'} );
        } elsif ( $item->{'type'} eq 'anchor' ) {
            $r = $c->add_reading( '#ANCHOR_' . $item->{'content'} . '#' );
            $r->is_meta(1);
        } elsif ( $item->{'type'} eq 'app' ) {
            my $tag = '#APP_' . $counter++ . '#';
            $r = $c->add_reading( $tag );
            $r->is_meta(1);
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
    expand_all_paths( $c );
}

sub _stringify_sigil {
    my( @nodes ) = @_;
    my @parts = grep { /\w/ } map { $_->data } @nodes;
    return join( '', @parts );
}

## Recursive little helper function to help us navigate through nested
## XML, picking out the words, the apparatus, and the anchors.

sub _get_base {
	my( $xn ) = @_;
	my @readings;
	if( $xn->nodeType == XML_TEXT_NODE ) {
	    # Base text, just split the words on whitespace and add them 
	    # to our sequence.
	    # TODO consider that XML markup might appear mid-token.
		my $str = $xn->data;
		$str =~ s/^\s+//;
		foreach my $w ( split( /\s+/, $str ) ) {
		    # HACK to cope with mismatched doublequotes
		    $w =~ s/\"//g;
			push( @readings, { 'type' => 'token', 'content' => $w } );
		}
	} elsif( $xn->nodeName eq 'hi' ) {
		# Recurse as if the hi weren't there.
		foreach( $xn->childNodes ) {
			push( @readings, _get_base( $_ ) );
		}
	} elsif( $xn->nodeName eq 'app' ) {
		# Apparatus, just save the entire XML node.
		push( @readings, { 'type' => 'app', 'content' => $xn } );
	} elsif( $xn->nodeName eq 'anchor' ) {
		# Anchor to mark the end of some apparatus; save its ID.
		push( @readings, { 'type' => 'anchor', 
		    'content' => $xn->getAttribute( 'xml:id' ) } );
	} elsif ( $xn->nodeName ne 'note' ) {  # Any tag we don't know to disregard
	    print STDERR "Unrecognized tag " . $xn->nodeName . "\n";
	}
	return @readings;
}

sub _add_readings {
    my( $c, $app_id ) = @_;
    my $xn = $apps{$app_id};
    my $anchor = _anchor_name( $xn->getAttribute( 'to' ) );
    # Get the lemma, which is all the readings between app and anchor,
    # excluding other apps or anchors.
    my @lemma = _return_lemma( $c, $app_id, $anchor );
    my $lemma_str = join( ' ', grep { $_ !~ /^\#/ } map { $_->text } @lemma );
    
    # For each reading, send its text to 'interpret' along with the lemma,
    # and then save the list of witnesses that these tokens belong to.
    my %wit_rdgs;  # Maps from witnesses to the variant text
    my $ctr = 0;
    my $tag = $app_id;
    $tag =~ s/^\#APP_(.*)\#$/$1/;
    $DB::single = 1 if $tag < 2;
    foreach my $rdg ( $xn->getChildrenByTagName( 'rdg' ) ) {
        my @text;
        foreach ( $rdg->childNodes ) {
            push( @text, _get_base( $_ ) );
        }
        my $interpreted = @text 
            ? interpret( join( ' ', map { $_->{'content'} } @text ), $lemma_str ) 
            : '';
        my @rdg_nodes;
        foreach my $w ( split( /\s+/, $interpreted ) ) {
            my $r = $c->add_reading( $tag . "/" . $ctr++ );
            $r->text( $w );
            push( @rdg_nodes, $r );
        }
        
        # For each listed wit, save the reading.
        foreach my $wit ( split( /\s+/, $rdg->getAttribute( 'wit' ) ) ) {
            $wit_rdgs{$wit} = \@rdg_nodes;
        }
        # Does the reading have an ID? If so it probably has a witDetail
        # attached, and we need to read it.
        if( $rdg->hasAttribute( 'xml:id' ) ) {
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
    foreach my $wit_id ( keys %wit_rdgs ) {
        my $witstr = get_sigil( $wit_id, $c );
        my $rdg_list = $wit_rdgs{$wit_id};
        _add_wit_path( $c, $rdg_list, $app_id, $anchor, $witstr );
    }
}

sub _anchor_name {
    my $xmlid = shift;
    $xmlid =~ s/^\#//;
    return sprintf( "#ANCHOR_%s#", $xmlid );
}

sub _return_lemma {
    my( $c, $app, $anchor ) = @_;
    my $app_node = $c->graph->node( $app );
    my $anchor_node = $c->graph->node( $anchor );
    my @nodes = grep { $_->name !~ /^\#A(PP|NCHOR)/ } 
        $c->reading_sequence( $app_node, $anchor_node, $c->baselabel );
    return @nodes;
}

sub interpret {
	# A utility function to change apparatus-ese into a full variant.
	my( $reading, $lemma ) = @_;
	return $reading if $reading eq $lemma;
	my $oldreading = $reading;
	# $lemma =~ s/\s+[[:punct:]]+$//;
	# $reading =~ s/\s*\(?sic([\s\w.]+)?\)?$//;
	my @words = split( /\s+/, $lemma );
	if( $reading =~ /^(.*) praem.$/ ) {
		$reading = "$1 $lemma";
	} elsif( $reading =~ /^(.*) add.$/ ) {
		$reading = "$lemma $1";
	} elsif( $reading eq 'om.' 
	    || $reading =~ /locus [uv]acuus/
	    || $reading =~ /inscriptionem compegi e/ # TODO huh?
	    || $reading eq 'def.' # TODO huh?
	    ) {
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

sub _parse_wit_detail {
    my( $detail, $readings, $lemma ) = @_;
    my $wit = $detail->getAttribute( 'wit' );
    my $content = $detail->textContent;
    if( $content =~ /a\.\s*c\./ ) {
        # Replace the key in the $readings hash
        my $rdg = delete $readings->{$wit};
        $readings->{$wit.'_ac'} = $rdg;
        $has_ac{$sigil_for{$wit}} = 1;
    } elsif( $content =~ /p\.\s*c\./ ) {
        # If no key for the wit a.c. exists, add one pointing to the lemma
        unless( exists $readings->{$wit.'_ac'} ) {
            $readings->{$wit.'_ac'} = $lemma;
        }
        $has_ac{$sigil_for{$wit}} = 1;
    } # else don't bother just yet
}

sub get_sigil {
    my( $xml_id, $c ) = @_;
    if( $xml_id =~ /^(.*)_ac$/ ) {
        my $real_id = $1;
        return $sigil_for{$real_id} . $c->ac_label;
    } else {
        return $sigil_for{$xml_id};
    }
}

sub expand_all_paths { 
    my( $c ) = @_;
    
    # Walk the collation and fish out the paths for each witness
    foreach my $wit ( $c->tradition->witnesses ) {
        my $sig = $wit->sigil;
        my @path = grep { $_->name !~ /(APP|ANCHOR)/ } 
            $c->reading_sequence( $c->start, $c->end, $sig );
        $wit->path( \@path );
        if( $has_ac{$sig} ) {
            my @ac_path = grep { $_->name !~ /(APP|ANCHOR)/ } 
                $c->reading_sequence( $c->start, $c->end, $sig.$c->ac_label, $sig );
            $wit->uncorrected_path( \@ac_path );
        }
    }   
    
    # Delete the anchors
    foreach my $anchor ( grep { $_->name =~ /(APP|ANCHOR)/ } $c->readings ) {
        $c->del_reading( $anchor );
    }
    # Delete all edges
    map { $c->del_path( $_ ) } $c->paths;
    
    # Make the path edges
    $c->make_witness_paths();
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

=back

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews, aurum@cpan.org

=cut

1;

