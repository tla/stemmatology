package Text::Tradition::Language::Latin;

use strict;
use warnings;
use Module::Load;
use Text::Tradition::Language::Base qw/ lemmatize_treetagger treetagger_struct /;
use TryCatch;

=head1 NAME

Text::Tradition::Language::Latin - language-specific module for Latin

=head1 DESCRIPTION

Implements morphology lookup for French words in context.  This module
depends on the Morph::Perseus module for access to PhiloLogic database data.
It also depends on the TreeTagger software
(L<http://www.ims.uni-stuttgart.de/projekte/corplex/TreeTagger/>), which is
(for now) expected to be installed in $MORPHDIR/TreeTagger.

=head1 SUBROUTINES

=head2 lemmatize( $text )

Evaluates the string using the Flemm package, and returns the results.

=begin testing

use Text::Tradition;
use_ok( 'Text::Tradition::Language::Latin' );

eval "use Morph::Perseus";
my $err = $@;

SKIP: {
	skip "Package Morph::Perseus not found" if $err;

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
			next if $l->is_disambiguated;
	 		printf( "Ambiguous lexeme %s for reading %s:\n\t%s\n", $l->string, $r->id,
	 			join( "\n\t", map { $_->lemma . ': ' . $_->morphology->to_string } $l->matching_forms ) );
			$ambig++;
		}
	}
	is( $ambig, 19, "Found 19 ambiguous forms as expected" );
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

{
	my $morph;
	
	sub _morph_connect {
		unless( $morph ) {
			try {
				load 'Morph::Perseus';
				load 'Morph::Perseus::Structure';
				$morph = Morph::Perseus->connect( 'Latin' );
			} catch {
				warn "Cannot do Latin word lemmatization without Morph::Perseus: @_";
				return;
			}
		}
	}
		
	sub _perseus_lookup_tt {
		my( $orig, $pos, $lemma ) = split( /\t/, $_[0] );
		_morph_connect();
		my $result = $morph->lookup( $orig );
		# Discard results that don't match the lemma, unless lemma is unknown
		my @ret;
		unless( $lemma eq '<unknown>' ) {
			# TODO Perseus lemma might have a number on the end, yuck.
			@ret = grep { $_->lemma =~ /^$lemma(\d*)$/ } @{$result->{'objects'}};
		}
		unless( @ret ) {
			@ret = @{$result->{'objects'}};
			warn "TreeTagger lemma $lemma matched no results from Perseus for $orig"
				if @ret;
		}
		
		# Discard results that don't match the given TreeTagger POS, unless
		# that leaves zero results
		my @wordforms;
		foreach my $obj ( @ret ) {
			push( @wordforms, _wordform_from_row( $obj ) );
		}
		## TODO Use TreeTagger info - requires serious hacking of Lingua::TagSet
# 		my $ttstruct = treetagger_struct( $pos );
# 		my @ttmatch = grep { $ttstruct->is_compatible( $_->morphology ) } @wordforms;
# 		unless( @ttmatch ) {
# 			warn "TreeTagger POS $pos matched no results from Perseus for $orig";
# 			@ttmatch = @wordforms;
# 		}
# 		return @ttmatch;
		return @wordforms;
	}
	
	sub _perseus_lookup_str {
		my( $orig ) = @_;
		_morph_connect();
		# Simple morph DB lookup, and return the results.
		my $result = $morph->lookup( $orig );
		return map { _wordform_from_row( $_ ) } @{$result->{'objects'}};
	}
	
	sub _wordform_from_row {
		my( $rowobj ) = @_;
		my $mpstruct = Morph::Perseus::Structure->from_tag( $rowobj->code );
		my $wf = Text::Tradition::Collation::Reading::WordForm->new(
			'language' => 'Latin',
			'lemma' => $rowobj->lemma,
			'morphology' => $mpstruct,
			);
		return $wf;
	}
	
}

1;