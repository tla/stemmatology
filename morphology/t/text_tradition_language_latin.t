#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use Text::Tradition;
use_ok( 'Text::Tradition::Language::Latin' );

my $trad = Text::Tradition->new(
	'language' => 'Latin',
	'file' => 't/data/legendfrag.xml',
	'input' => 'Self' );

eval "use Lingua::Morph::Perseus";
my $err = $@;
SKIP: {
	skip "Package Lingua::Morph::Perseus not found" if $err;

	$trad->lemmatize();
	my $ambig = 0;
	foreach my $r ( $trad->collation->readings ) {
		next if $r->is_meta;
		ok( $r->has_lexemes, "Reading $r has one or more lexemes" );
		my @lex = $r->lexemes;
		my $lexstr = join( '', map { $_->string } @lex );
		my $textstr = $r->text;
		$textstr =~ s/\s+//g;
		is( $textstr, $lexstr, "Lexemes for reading $r match the reading" );
		foreach my $l ( @lex ) {
			next unless $l->matches;
			next if $l->is_disambiguated;
	 		printf( "Ambiguous lexeme %s for reading %s:\n\t%s\n", $l->string, $r->id,
	 			join( "\n\t", map { $_->lemma . ': ' . $_->morphology->to_string } $l->matching_forms ) );
			$ambig++;
		}
	}
	is( $ambig, 4, "Found 4 ambiguous forms as expected" );
}
	
# Try exporting some witnesses
my $e_v = 'in suetia uenerabilis pontifex beatus henricus in anglia oriundus';
my $struct_v = $trad->witness('V')->export_as_json;
my $g_v = join( ' ', map { $_->{'n'} } @{$struct_v->{'tokens'}} );
is( $g_v, $e_v, "Got expected regularization of witness V" );
my $e_n = 'in suetia beatus henricus uenerabilis pontifex de anglia oriundus';
my $struct_n = $trad->witness('N')->export_as_json;
my $g_n = join( ' ', map { $_->{'n'} } @{$struct_n->{'tokens'}} );
is( $g_n, $e_n, "Got expected regularization of witness N" );
}




1;
