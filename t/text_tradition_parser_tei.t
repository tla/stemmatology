#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use Text::Tradition;
binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";
eval { no warnings; binmode $DB::OUT, ":utf8"; };

my $par_seg = 't/data/florilegium_tei_ps.xml';
my $t = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'TEI',
    'file'  => $par_seg,
    );

is( ref( $t ), 'Text::Tradition', "Parsed parallel-segmentation TEI" );
if( $t ) {
    is( scalar $t->collation->readings, 319, "Collation has all readings" );
    is( scalar $t->collation->paths, 374, "Collation has all paths" );
}
}



# =begin testing
{
use XML::LibXML;
use XML::LibXML::XPathContext;
use Text::Tradition::Parser::TEI;

my $xml_str = '<tei><rdg wit="#A #B #C #D">some text</rdg></tei>';
my $el = XML::LibXML->new()->parse_string( $xml_str )->documentElement;
my $xpc = XML::LibXML::XPathContext->new( $el );
my $obj = $xpc->find( '//rdg' );

my @wits = Text::Tradition::Parser::TEI::_get_sigla( $obj );
is( join( ' ', @wits) , "A B C D", "correctly parsed reading wit string" );
}




1;
