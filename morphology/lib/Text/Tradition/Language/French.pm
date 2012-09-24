package Text::Tradition::Language::French;

use strict;
use warnings;
use Lingua::TagSet::Multext;
use Lingua::TagSet::TreeTagger::French;
use Module::Load qw/ load /;
use Text::Tradition::Language::Base qw/ lemmatize_treetagger reading_lookup_treetagger 
	lfs_morph_tags unicode_regularize /;
use TryCatch;

=head1 NAME

Text::Tradition::Language::French - language-specific module for French

=head1 DESCRIPTION

Implements morphology lookup for French words in context.  This module
depends on the Flemm module for French lemmatization
(L<http://www.univ-nancy2.fr/pers/namer/Outils.htm#fl3> in conjunction with
the TreeTagger software
(L<http://www.ims.uni-stuttgart.de/projekte/corplex/TreeTagger/>), which is
(for now) expected to be installed in $MORPHDIR/TreeTagger.

=head1 SUBROUTINES

=head2 lemmatize( $text )

Evaluates the string using the Flemm package, and returns the results.

=begin testing

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
	my $flemmed = 0;
	foreach my $r ( $tf->collation->readings ) {
		next if $r->is_meta;
		ok( $r->has_lexemes, "Reading $r has one or more lexemes" );
		my @lex = $r->lexemes;
		my $lexstr = join( '', map { $_->string } @lex );
		my $textstr = $r->text;
		$textstr =~ s/\s+//g;
		is( $textstr, $lexstr, "Lexemes for reading $r match the reading" );
		foreach my $l ( @lex ) {
			# Check to see if Flemm actually ran
			foreach my $wf ( $l->matching_forms ) {
				$flemmed++ if $wf->morphology->get_feature('num');
			}
			next if $l->is_disambiguated;
			$ambig++;
		}
	}
	is( $ambig, 102, "Found 102 ambiguous forms as expected" );
	ok( $flemmed > 500, "Found enough Flemm info in wordforms" );
	
	# Try setting the normal form of a reading and re-analyzing
	my $mr = $tf->collation->reading('r99.2');
	is( $mr->text, 'minspire', "Picked correct test reading" );
	is( $mr->language, 'French', "Reading has correct language setting" );
	$mr->normal_form( "m'inspire" );
	$mr->lemmatize;
	my @l = $mr->lexemes;
	is( @l, 2, "Got two lexemes for new m'inspire reading" );
	is( $l[0]->form->to_string,
		'"French // se|le|lui // cat@pron type@pers pers@1 num@sing case@acc|dat"',
		"New reading has correct first lexeme" );
}

=end testing

=cut

sub lemmatize {
	my $tradition = shift;
	my %opts = ( 
		'language' => 'French', 
		'callback' => sub { _parse_wordform( _flemm_lookup( @_ ) ) } 
		);
	return lemmatize_treetagger( $tradition, %opts );
}

=head2 reading_lookup( $rdg[, $rdg, ...] )

Looks up one or more readings using the Flemm package, and returns the
possible results.  This uses the same logic as L<lemmatize> above for the
entire tradition, but can also be used to (re-)analyze individual readings.

=cut

sub reading_lookup {
	my( @path ) = @_;
	my %opts = ( 
		'language' => 'French',
		'callback' => sub { _parse_wordform( _flemm_lookup( @_ ) ) },
		'path' => \@path,
		);
	return reading_lookup_treetagger( %opts );
}

=head2 morphology_tags

Return a data structure describing the available parts of speech and their attributes.

=cut

sub morphology_tags {
	return lfs_morph_tags();
}

# Closure and utility function for the package lemmatizer
{
	my $lemmatizer;
	
	sub _flemm_lookup {
		# First try to load Flemm
		unless( $lemmatizer ) {
			try {
				load 'Flemm';
				$lemmatizer = Flemm->new( 'Encoding' => 'utf8', 'Tagger' => 'treetagger' );
			} catch {
				warn "Cannot do French word lemmatization without Flemm: @_";
				return;
			}
		}
		return $lemmatizer->lemmatize( @_ )
	}
	
}

# Utility function to turn a Flemm result into a WordForm
sub _parse_wordform {
	my $flemmobj = shift;
	# For now just parse the string, until we make sense of the documentation.
	my @results = split( / \|\| /, $flemmobj->getResult );
	my @forms;
	foreach ( @results ) {
		my( $orig, $tag, $lemma ) = split( /\t/, $_ );
		my( $pos, $morph ) = split( /:/, $tag );
		my $morphobj;
		if( $morph ) {
			$morphobj = Lingua::TagSet::Multext->tag2structure( $morph );
		} else {
			# Use the TreeTagger info if there is no Flemm morphology.
			$morphobj = Lingua::TagSet::TreeTagger::French->tag2structure( $pos );
		}
		if( $morphobj ) {
			my $wf = Text::Tradition::Collation::Reading::WordForm->new(
				'language' => 'French',
				'lemma' => $lemma,
				'morphology' => $morphobj,
				);
			push( @forms, $wf );
		} else {
			warn "No morphology found for word: $_";
		}
	}
	return @forms;
}

=head2 regularize( $text )

Returns a regularized form of the reading for the purposes of collation.

=cut

sub regularize {
	return unicode_regularize( @_ );
}

1;

=head2 TODO

=over

=item * Try to do more things with Perl objects in Flemm and TT

=back

