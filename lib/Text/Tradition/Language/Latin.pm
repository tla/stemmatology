package Text::Tradition::Language::Latin;

use strict;
use warnings;
use Module::Load;
use Text::Tradition::Language::Base qw/ lemmatize_treetagger lfs_morph_tags /;
use TryCatch;

=head1 NAME

Text::Tradition::Language::Latin - language-specific module for Latin

=head1 DESCRIPTION

Implements morphology lookup for French words in context.  This module
depends on the Lingua::Morph::Perseus module for access to PhiloLogic database data.

=head1 SUBROUTINES

=head2 lemmatize( $text )

Evaluates the string using the Flemm package, and returns the results.

=begin testing

use Text::Tradition;
use_ok( 'Text::Tradition::Language::Latin' );

eval "use Lingua::Morph::Perseus";
my $err = $@;

SKIP: {
	skip "Package Lingua::Morph::Perseus not found" if $err;

	my $trad = Text::Tradition->new(
		'language' => 'Latin',
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

sub lemmatize {
	my $tradition = shift;
	my %opts = ( 
		'language' => 'Latin', 
		'callback' => sub { _perseus_lookup_tt( @_ ) } 
		);
	return lemmatize_treetagger( $tradition, %opts );
}

=head2 reading_lookup( $rdg[, $rdg, ...] )

Looks up one or more readings using the Perseus package, and returns the
possible results.  This skips the tree tagger / tokenizer, returning any
match for the word string(s) in the morphology DB.

=cut

sub reading_lookup {
	my @words = @_;
	return map { _perseus_lookup_str( $_ ) } @words;
}

=head2 morphology_tags

Return a data structure describing the available parts of speech and their attributes.

=cut

sub morphology_tags {
	return lfs_morph_tags();
}


{
	my $morph;
	
	sub _morph_connect {
		unless( $morph ) {
			try {
				load 'Lingua::Morph::Perseus';
				$morph = Lingua::Morph::Perseus->connect( 'Latin' );
			} catch {
				warn "Cannot do Latin word lemmatization without Lingua::Morph::Perseus: @_";
				return;
			}
		}
	}
				
	sub _perseus_lookup_tt {
		my( $orig, $pos, $lemma ) = split( /\t/, $_[0] );
		_morph_connect();
		return unless $morph;
		# Discard results that don't match the lemma, unless lemma is unknown
		my $lookupopts = {};
		unless( $lemma eq '<unknown>' || $lemma =~ /^\W+$/ ) {
			my %lems;
			map { $lems{$_} = 1; $lems{lc($_)} = 1 } split( /\|/, $lemma );
			$lookupopts->{'lemma'} = [ keys %lems ];
		}
		$lookupopts->{'ttpos'} = $pos if $pos;
		
		my $result = $morph->lexicon_lookup( $orig, $lookupopts );
		# unless( !keys( %$lookupopts ) ||  $result->{'filtered'} ) {
		# 	warn "Filter on $pos / $lemma returned no results; using all results";
		# }
		my @ret = @{$result->{'objects'}};
		my %unique_wordforms;
		foreach my $obj ( @ret ) {
			my $wf = _wordform_from_row( $obj );
			$unique_wordforms{$wf->to_string} = $wf;
		}
		return values( %unique_wordforms );
	}
	
	sub _perseus_lookup_str {
		my( $orig ) = @_;
		_morph_connect();
		return unless $morph;
		# Simple morph DB lookup, and return the results.
		my $result = $morph->lookup( $orig );
		return map { _wordform_from_row( $_ ) } @{$result->{'objects'}};
	}
	
}

sub _wordform_from_row {
	my( $rowobj ) = @_;
	my $lemma = $rowobj->lemma;
	$lemma =~ s/^(\D+)\d*$/$1/;
	my $wf = Text::Tradition::Collation::Reading::WordForm->new(
		'language' => 'Latin',
		'lemma' => $lemma,
		'morphology' => $rowobj->morphology,
		);
	return $wf;
}
	
1;