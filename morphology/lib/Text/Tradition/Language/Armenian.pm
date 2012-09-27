package Text::Tradition::Language::Armenian;

use strict;
use warnings;
use Module::Load;
use parent qw/ Text::Tradition::Language::Perseus /;

=head1 NAME

Text::Tradition::Language::Armenian - language-specific module for Armenian

=head1 DESCRIPTION

Implements morphology lookup for Armenian (Grabar) words in context.  This module
depends on the Lingua::Morph::Perseus module for access to PhiloLogic database data.

=head1 SUBROUTINES

=head2 lemmatize( $text )

Evaluates the string using Treetagger and Perseus, and returns the results.

=head2 reading_lookup( $word )

Returns a single-word morphological lookup of the given word using Perseus.

=begin testing

use Text::Tradition;
use_ok( 'Text::Tradition::Language::Armenian' );

eval "use Lingua::Morph::Perseus";
my $err = $@;

SKIP: {
	skip "No Armenian test data yet";

	my $trad = Text::Tradition->new(
		'language' => 'Armenian',
		'file' => 't/data/legendfrag.xml',
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
	my( $word ) = @_;
	# Get rid of accent marks.
	$word =~ s/՛//g;
	# Get rid of hyphen.
	$word =~ s/֊//g;
	# Get rid of any backtick that falls mid-word.
	$word =~ s/՝(.)/$1/g;
	# Standardize ligatures.
	$word =~ s/աւ/օ/g;	# for easy vocalic comparison to ո
	$word =~ s/և/եւ/g;
	
	# Downcase the word.
	$word = lc( $word );
	return $word;
}

1;

