#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
binmode STDOUT, ':utf8';
use Text::Tradition;
use_ok( 'Text::Tradition::Language::French' );

eval "use Flemm";
my $err = $@;

SKIP: {
	skip "Package Flemm not found" if $err;
	my $tf = Text::Tradition->new(
		'input' => 'Self',
		'file' => 't/data/besoin.xml',
		'language' => 'French' );
		
	is( $tf->language, 'French', "Set language okay" );
	$tf->lemmatize();
	# Test the lemmatization. How many readings now have morphological info?
	# Do the lexemes match the reading?
	my $ambig = 0;
	foreach my $r ( $tf->collation->readings ) {
		next if $r->is_meta;
		ok( $r->has_lexemes, "Reading $r has one or more lexemes" );
		my @lex = $r->lexemes;
		my $lexstr = join( '', map { $_->string } @lex );
		my $textstr = $r->text;
		$textstr =~ s/\s+//g;
		is( $textstr, $lexstr, "Lexemes for reading $r match the reading" );
		foreach my $l ( @lex ) {
			next if $l->is_disambiguated;
	# 		printf( "Ambiguous lexeme %s for reading %s:\n\t%s\n", $l->string, $r->id,
	# 			join( "\n\t", map { $_->lemma . ': ' . $_->morphology->to_string } $l->matching_forms ) );
			$ambig++;
		}
	}
	is( $ambig, 102, "Found 102 ambiguous forms as expected" );
	
	# Try setting the normal form of a reading and re-analyzing
	my $mr = $tf->collation->reading('r99.2');
	is( $mr->text, 'minspire', "Picked correct test reading" );
	is( $mr->language, 'French', "Reading has correct language setting" );
	$mr->normal_form( "m'inspire" );
	$mr->lemmatize;
	is( $mr->lexemes, 2, "Got two lexemes for new m'inspire reading" );
}
}




1;
