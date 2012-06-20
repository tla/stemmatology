#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use Text::Tradition;
use_ok( 'Text::Tradition::Language::Greek' );

eval "use Lingua::Morph::Perseus";
my $err = $@;

SKIP: {
	skip "Greek linguistic data not read yet";

	my $trad = Text::Tradition->new(
		'language' => 'Greek',
		'file' => 't/data/florilegium_graphml.xml',
		'input' => 'Self' );
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
}




1;
