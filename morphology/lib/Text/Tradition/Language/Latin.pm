package Text::Tradition::Language::Latin;

use strict;
use warnings;
use Module::Load;
use parent qw/ Text::Tradition::Language::Perseus /;

=head1 NAME

Text::Tradition::Language::Latin - language-specific module for Latin

=head1 DESCRIPTION

Implements morphology lookup for Latin words in context.  This module
depends on the Lingua::Morph::Perseus module for access to PhiloLogic database data.

=head1 SUBROUTINES

=head2 lemmatize( $text )

Evaluates the string using Treetagger and Perseus, and returns the results.

=head2 reading_lookup( $word )

Returns a single-word morphological lookup of the given word using Perseus.

=begin testing

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

# TODO Check this against Perseus regularization standards

sub regularize {
	my( $word ) = @_;
	$word = lc( $word );
	$word =~ s/v/u/g;
	$word =~ s/w/u/g;
	$word =~ s/j/i/g;
	return $word;
}

=head2 collation_normalize( $text )

Returns a normalized form of the reading for the purposes of collation.

=cut

sub collation_normalize {
	my( $word ) = @_;
	$word = lc( $word );
	$word =~ s/v/u/g;
	$word =~ s/w/u/g;
	$word =~ s/j/i/g;
	$word =~ s/ci/ti/g;
	$word =~ s/cha/ca/g;
	return $word;
}

1;

