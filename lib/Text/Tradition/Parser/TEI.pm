package Text::Tradition::Parser::TEI;

use strict;
use warnings;
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

sub parse {
    my( $tradition, $xml_str ) = @_;
    
    # First, parse the XML.
    my $parser = XML::LibXML->new();
    my $doc = $parser->parse_string( $xml_str );
    my $tei = $doc->documentElement();
    my $xpc = XML::LibXML::XPathContext->new( $tei );
    $xpc->registerNs( 'tei', 'http://www.tei-c.org/ns/1.0' );
    
    # Then get the witnesses and create the witness objects.
    foreach my $wit_el ( $xpc->findnodes( '//tei:listWit/tei:witness' ) ) {
	my $sig = $wit_el->getAttribute( 'xml:id' );
	my $source = $wit_el->toString();
	$tradition->add_witness( sigil => $sig, source => $source );
    }

    # Now go through the text and make the tokens.
    # Assume for now that each word is tokenized in the XML.
    my $text = {};
    map { $text->{$_->sigil} = [] } @{$tradition->witnesses};
    my $word_ctr = 0;
    my %used_word_ids;
    foreach my $word_el ( $xpc->findnodes( '//tei:w|tei:seg' ) ) {
	# If it is contained within a lem or a rdg, look at those witnesses.
	# Otherwise it is common to all witnesses.
	# Also common if it is the only lem/rdg within its app.
	# Thus we are assuming non-nested apps.
	    
	my $parent_rdg = $xpc->find( 'parent::tei:lem|parent::tei:rdg', $word_el );
	my @wits = get_sigla( $parent_rdg );
	@wits = map { $_->sigil } @{$tradition->witnesses} unless @wits;

	# Create the node
	my $reading = make_reading( $tradition->collation, $word_el );

	# Figure out if it is a common node, that is, if it is outside an apparatus
	# or the only rdg in an apparatus
	my $common = 1;
	if( $xpc->findnodes( 'ancestor::tei:app', $word_el ) ) {
	    # If we are in an app we are not a common node...
	    $common = 0;
	    if( $xpc->findnodes( 'ancestor::tei:app/tei:rdg' )->size == 1 ) {
		# unless we are the only reading in the app.
		$common = 1;
	    }
	}
	$reading->make_common if $common;
	
	foreach my $sig ( @wits ) {
	    push( @{$text->{$sig}}, $reading );
	}
    }

    $DB::single = 1;
    # Now we have the text paths through the witnesses, so we can make
    # the edges.
    my $end = $tradition->collation->add_reading( '#END#' );
    foreach my $sigil ( keys %$text ) {
	my @nodes = @{$text->{$sigil}};
	my $source = $tradition->collation->start;
	foreach my $n ( @nodes ) {
	    # print STDERR sprintf( "Joining %s -> %s for wit %s\n", $source->text, $n->text, $sigil );
	    $tradition->collation->add_path( $source, $n, $sigil );
	    $source = $n;
	}
	$tradition->collation->add_path( $source, $end, $sigil );
    }

    # TODO think about relationships, transpositions, etc.
}

sub get_sigla {
    my( $rdg ) = @_;
    # Cope if we have been handed a NodeList.  There is only
    # one reading here.
    if( ref( $rdg ) eq 'XML::LibXML::NodeList' ) {
	$rdg = $rdg->shift;
    }

    my @wits;
    if( ref( $rdg ) eq 'XML::LibXML::Element' ) {
	@wits = split( /\s+/, $rdg->getAttribute( 'wit' ) );
	map { $_ =~ s/^\#// } @wits;
    }
    return @wits;
}

{
    my $word_ctr = 0;
    my %used_nodeids;

    sub make_reading {
	my( $graph, $word_el) = @_;
	my $xml_id = $word_el->getAttribute( 'xml:id' );
	if( $xml_id && exists $used_nodeids{$xml_id} ) {
	    warn "Already used assigned ID $xml_id";
	    $xml_id = undef;
	}
	if( !$xml_id ) {
	    until( $xml_id ) {
		my $try_id = 'w'.$word_ctr++;
		next if exists $used_nodeids{$try_id};
		$xml_id = $try_id;
	    }
	}
	my $rdg = $graph->add_reading( $xml_id );
	$rdg->text( $word_el->textContent() );
	$used_nodeids{$xml_id} = $rdg;
	return $rdg;
    }
}

1;
