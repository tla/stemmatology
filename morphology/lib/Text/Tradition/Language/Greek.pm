package Text::Tradition::Language::Greek;

use strict;
use warnings;
use Module::Load;
use parent qw/ Text::Tradition::Language::Perseus /;
use Text::Tradition::Language::Base qw/ unicode_regularize /;

=head1 NAME

Text::Tradition::Language::Greek - language-specific module for Greek

=head1 DESCRIPTION

Implements morphology lookup for Greek words in context.  This module
depends on the Lingua::Morph::Perseus module for access to PhiloLogic database data.

=head1 SUBROUTINES

=head2 lemmatize( $text )

Evaluates the string using Treetagger and Perseus, and returns the results.

=head2 reading_lookup( $word )

Returns a single-word morphological lookup of the given word using Perseus.

=begin testing

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

=end testing

=cut

our $dbhandle;

sub lemmatize {
	return __PACKAGE__->perseus_lemmatize( @_ );
}

sub reading_lookup {
	return __PACKAGE__->perseus_reading_lookup( @_ );
}

=head2 regularize( $text )

Returns an orthographically regular form of the reading.

=cut

sub regularize {
	return Text::Tradition::Language::Base::regularize( @_ );
}

=head2 collation_normalize( $text )

Returns a normalized form of the reading for the purposes of collation.

=cut

sub collation_normalize {
	return unicode_regularize( @_ );
}

1;

