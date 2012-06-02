package Text::Tradition::Language::Latin;

use strict;
use warnings;
use Module::Load;
use Morph::Perseus::Structure;
use Text::Tradition::Language::Base qw/ lemmatize_treetagger treetagger_struct 
	lfs_morph_tags /;
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
			next unless $l->matches;
			next if $l->is_disambiguated;
	 		printf( "Ambiguous lexeme %s for reading %s:\n\t%s\n", $l->string, $r->id,
	 			join( "\n\t", map { $_->lemma . ': ' . $_->morphology->to_string } $l->matching_forms ) );
			$ambig++;
		}
	}
	is( $ambig, 7, "Found 7 ambiguous forms as expected" );
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
	try {
		load 'Morph::Perseus::Structure';
	} catch {
		warn "Not using Perseus Latin tags";
	}
	return lfs_morph_tags();
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
		
	# TODO special case:
	#  passive verbs (-or)
	#  T sapientia -> sapientia
	#  T primus -> unus
	#  T occulta -> occultus (with occulo in next field, hmm...)
	#  T carne -> carnis
	#  T melius -> bonus
	

	my %excep = (
		'absens' => 'absum',
		'aperte' => 'apertus',
		'evolvo' => 'exvolvo',
		'inquiam' => 'inquam',
		'intelligo' => 'intellego',
		'itaque' => 'ita',
		'iuste' => 'iustus',
		'longe' => 'longus',
		'male' => 'malus|malum',
		'multum' => 'multus',
		'nec' => 'neque',
		'nos' => 'ego',
		'occultum' => 'occultus',
		'peregrinans' => 'peregrinor',
		'perfectus' => 'perficio',
		'potius' => 'potis',
		'praesente' => 'praesens',
		'prius' => 'prior',
		'quotidianus' => 'cottidianus',
		'se' => 'sui',
		'septem' => 'septimus',
		'Spiritum' => 'spiritus',
		'viriliter' => 'virilis', # TODO special case -iter?
		'vos' => 'tu',
		
		'datum' => 'do|data|datus',
		'forte' => 'fors|fortis',
		'vere' => 'verum|verus',
		);
		
	sub _perseus_lookup_tt {
		my( $orig, $pos, $lemma ) = split( /\t/, $_[0] );
		_morph_connect();
		my $result = $morph->lookup( $orig );
		# Discard results that don't match the lemma, unless lemma is unknown
		my @orig = @{$result->{'objects'}};
		my @ret;
		unless( $lemma eq '<unknown>' || $lemma =~ /^\W+$/ ) {
			# TODO Perseus lemma might have a number on the end, yuck.
			#  multiple lemmata separated with |
			$lemma =~ s/[^\w|]//g;
			$lemma = $excep{$lemma} if exists $excep{$lemma};
			$lemma =~ s/j/i/g;
			my %lems;
			my @forms = 
			map { $lems{$_} = 1; $lems{lc($_)} = 1 } split( /\|/, $lemma );
			# Now match the lemmata from Treetagger to the lemmata and alt_ls 
			# from Perseus.
			@ret = grep { 
				my $x = $_->lemma; 
				$x =~ s/\d+$//;
				my $y = $_->alt_lex || '';
				$y =~ s/\d+$//;
				$lems{$x} || $lems{$y};
				} @orig;
			warn "TreeTagger lemma $lemma matched no results from Perseus for $orig"
				if @orig && !@ret;
		}
		@ret = @orig unless @ret;
		
		my %unique_wordforms;
		foreach my $obj ( @ret ) {
			my $wf = _wordform_from_row( $obj );
			$unique_wordforms{$wf->to_string} = $wf;
		}
		## TODO Use TreeTagger info - requires serious hacking of Lingua::TagSet
		# Discard results that don't match the given TreeTagger POS, unless
		# that leaves zero results
# 		my $ttstruct = treetagger_struct( $pos );
# 		my @ttmatch = grep { $ttstruct->is_compatible( $_->morphology ) } @wordforms;
# 		unless( @ttmatch ) {
# 			warn "TreeTagger POS $pos matched no results from Perseus for $orig";
# 			@ttmatch = @wordforms;
# 		}
# 		return @ttmatch;
		return values( %unique_wordforms );
	}
	
	sub _perseus_lookup_str {
		my( $orig ) = @_;
		_morph_connect();
		# Simple morph DB lookup, and return the results.
		my $result = $morph->lookup( $orig );
		return map { _wordform_from_row( $_ ) } @{$result->{'objects'}};
	}
	
}

sub _wordform_from_row {
	my( $rowobj ) = @_;
	my $mpstruct;
	try {
		$mpstruct = Morph::Perseus::Structure->from_tag( $rowobj->code );
	} catch {
		warn "Could not create morphology structure from "
			. $rowobj->code . ": $!";
	}
	my $lemma = $rowobj->lemma;
	$lemma =~ s/^(\D+)\d*$/$1/;
	my $wf = Text::Tradition::Collation::Reading::WordForm->new(
		'language' => 'Latin',
		'lemma' => $lemma,
		'morphology' => $mpstruct,
		);
	return $wf;
}
	
1;