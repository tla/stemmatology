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
    $xpc = XML::LibXML::XPathContext->new( $tei );
    $xpc->registerNs( 'tei', 'http://www.tei-c.org/ns/1.0' );
    
    # Then get the witnesses and create the witness objects.
    foreach my $wit_el ( $xpc->findnodes( '//tei:listWit/tei:witness' ) ) {
	my $sig = $wit_el->getAttribute( 'xml:id' );
	my $source = $wit_el->toString();  # Save all the XML info we have
	$tradition->add_witness( sigil => $sig, source => $source );
    }

    # Now go through the text and make the tokens.
    # Assume for now that each word is tokenized in the XML.
    my $text = {};
    map { $text->{$_->sigil} = [ $tradition->start ] } @{$tradition->witnesses};
    foreach my $word_el ( $xpc->findnodes( '//tei:w|tei:seg' ) ) {
	# If it is contained within a lem or a rdg, look at those witnesses.
	# Otherwise it is common to all witnesses.
	# Also common if it is the only lem/rdg within its app.
	# Thus we are assuming non-nested apps.
	my $node_id = $word_el->getAttribute( 'xml:id' );
	my $parent_rdg = $xpc->find( 'parent::tei:lem|parent::tei:rdg', $word_el );
	my @wits = get_sigla( $parent_rdg );
	@wits = map { $_->sigil } @{$tradition->witnesses} unless $wits;

	# TODO Create the node
	my $reading = $word_el->textContent();

	# TODO Figure out if it is a common node

	foreach my $sig ( @wits ) {
	    push( @{$text->{$sig}}, $reading );
	}
    }

    # Now we have the text paths through the witnesses, so we can make
    # the edges.

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
	@wits = split( /\s+/, $rdg->get_attribute( 'wit' ) );
	map { $_ =~ s/^\#// } @wits;
    }
    return @wits;
}

1;
