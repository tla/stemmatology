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

sub parse {
	my( $tradition, $xml_str ) = @_;
	my $c = $tradition->collation;	# Some shorthand
	
	# First, parse the XML.
	my $parser = XML::LibXML->new();
	my $doc = $parser->parse_string( $xml_str );
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
	my $counter = 0;
	my $last = $c->start;
	foreach my $item ( @base_text ) {
	    my $r;
        if( $item->{'type'} eq 'token' ) {
            $r = $c->add_reading( 'n'.$counter++ );
            $r->text( $item->{'content'} );
        } elsif ( $item->{'type'} eq 'anchor' ) {
            $r = $c->add_reading( '#ANCHOR_' . $item->{'content'} . '#' );
        } elsif ( $item->{'type'} eq 'app' ) {
            my $tag = '#APP_' . $counter++ . '#';
            $r = $c->add_reading( $tag );
            $apps{$tag} = $item->{'content'};
        }
        $c->add_path( $last, $r, 'BASE' );
        $last = $r;
    }
    $c->add_path( $last, $c->end, 'BASE' );
    
    # Now we can parse the apparatus entries, and add the variant readings 
    # to the graph.
    
    foreach my $app_id ( keys %apps ) {
        _add_readings( $c, $app_id );
    }
    
    # With the variant readings added, we now have to walk the graph for
    # each witness and add an explicit path wherever there is not a divergence
    # from BASE.  Thus we will also construct $wit->path.
	$DB::single = 1;
    foreach my $wit ( $tradition->witnesses ) {
        my $sig = $wit->sigil;
        my @wit_path = $c->reading_sequence( $c->start, $c->end, $sig, 'BASE' );
        my $cur = $c->start;
        foreach my $n ( @wit_path ) {
            next if $cur eq $c->start;
            my @paths = $cur->edges_to( $n );
            unless( grep { $_->name eq $sig } @paths ) {
                $c->add_path( $cur, $n, $sig );
            }
        }
        $wit->path( \@wit_path );
    }       
    
    # Collated readings are now on the graph, so now we get to remove
    # all BASE edges and all app/anchor nodes.
    foreach my $p ( $c->paths ) {
        $c->del_path( $p ) if $p->name eq 'BASE';
    }
    foreach my $n ( $c->readings ) {
        if( $n->name =~ /^\#A(PP|NCHOR)/ ) {
            # Pair up incoming / outgoing edges with the same label
            my( %incoming, %outgoing );
            foreach my $e ( $n->incoming ) {
                $incoming{$e->name} = $e->from;
                $c->del_path( $e );
            }
            foreach my $e ( $n->outgoing ) {
                $outgoing{$e->name} = $e->to;
                $c->del_path( $e );
            }
            foreach my $w ( keys %incoming ) {
                my $from = $incoming{$w};
                my $to = delete $outgoing{$w};
                warn "No outgoing edge on ".$n->name." for wit $w" unless $to;
                $c->add_path( $from, $to, $w );
            }
            foreach my $w ( keys %outgoing ) {
                warn "Found no incoming edge on ".$n->name." for wit $w";
            }
            $c->del_reading( $n );
        }
    }
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
		my $str = $xn->data;
		$str =~ s/^\s+//;
		foreach my $w ( split( /\s+/, $str ) ) {
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
    my %wit_details; # Maps from witnesses to the witness detail e.g. a.c.
    my $ctr = 0;
    my $tag = $app_id;
    $tag =~ s/^\#APP_(.*)\#$/$1/;
    foreach my $rdg ( $xn->getChildrenByTagName( 'rdg' ) ) {
        my @text;
        my $wits = $rdg->getAttribute( 'wit' );
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
        $wit_rdgs{$wits} = \@rdg_nodes;
        # Does the reading have an ID? If so it probably has a witDetail
        # attached, and that may be something we need to know.  For now,
        # save the reading ID.
        if( $rdg->hasAttribute( 'xml:id' ) ) {
        	$wit_details{$wits} = $rdg->getAttribute( 'xml:id' );
        }
    }
    # Now go through the available witDetails and, er, do something
    # foreach my $d ( $xn->getChildrenByTagName( 'witDetail' ) ) {
    	# my $referent = 
    
    # Now collate the variant readings, since it is not done for us.
    collate_variants( $c, \@lemma, values %wit_rdgs );
    
    # Now add the witness paths for each reading.
    foreach my $wit_str ( keys %wit_rdgs ) {
        my @wits = get_sigla( $wit_str );
        my $rdg_list = $wit_rdgs{$wit_str};
        _add_wit_path( $c, $rdg_list, $app_id, $anchor, @wits );
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
    my @nodes = grep { $_->name !~ /^\#A(PP|NCHOR)$/ } 
        $c->reading_sequence( $app_node, $anchor_node, 'BASE' );
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

sub get_sigla {
    my $witstr = shift;
    my @xml_ids = split( /\s+/, $witstr );
    my @sigs = map { $sigil_for{$_} } @xml_ids;
    return @sigs;
}

sub _add_wit_path {
    my( $c, $rdg, $app, $anchor, @wits ) = @_;
    my @nodes = @$rdg;
    push( @nodes, $c->graph->node( $anchor ) );
    
    my $cur = $c->graph->node( $app );
    foreach my $n ( @nodes ) {
        foreach my $w ( @wits ) {
            $c->add_path( $cur, $n, $w );
        }
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

