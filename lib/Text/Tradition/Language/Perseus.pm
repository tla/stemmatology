package Text::Tradition::Language::Perseus;

use strict;
use warnings;
use Module::Load;
use Text::Tradition::Language::Base qw/ lemmatize_treetagger reading_lookup_treetagger
	 lfs_morph_tags /;
use TryCatch;

=head1 NAME

Text::Tradition::Language::Perseus - base module for those languages that rely
on a Lingua::Morph::Perseus database.

=head1 DESCRIPTION

Implements morphology lookup for words in context.  This module depends on the Lingua::Morph::Perseus module for access to PhiloLogic database data.

=head1 SUBROUTINES

=head2 lemmatize( $text )

Evaluates the string using Treetagger and Perseus, and returns the results.

=cut

# tested in child language modules

sub perseus_lemmatize {
	my $self = shift;
	my $tradition = shift;
	my %opts = ( 
		'language' => $tradition->language, 
		'callback' => sub { _perseus_lookup_tt( $self, @_ ) } 
		);
	return lemmatize_treetagger( $tradition, %opts );
}

=head2 reading_lookup( $rdg[, $rdg, ...] )

Looks up one or more readings using the Perseus package, and returns the
possible results.  This skips the tree tagger / tokenizer, returning any
match for the word string(s) in the morphology DB.

=cut

sub perseus_reading_lookup {
	my( $self, @words ) = @_;
	my %opts = ( 
		'language' => $self->_get_lang(),
		'callback' => sub { _perseus_lookup_str( $self, @_ ) },
		'path' => \@words,
		);
	return reading_lookup_treetagger( %opts );
}

=head2 morphology_tags

Return a data structure describing the available parts of speech and their attributes.

=cut

sub morphology_tags {
	return lfs_morph_tags();
}

sub _get_lang {
	my $self = shift;
	my @parts = split( /::/, $self );
	return $parts[-1];
}

sub _morph_connect {
	my $self = shift;
	unless( $self::dbhandle ) {
		my $lang = $self->_get_lang();
		try {
			load 'Lingua::Morph::Perseus';
			$self::dbhandle = Lingua::Morph::Perseus->connect( $lang );
		} catch {
			warn "Cannot do $lang word lemmatization without Lingua::Morph::Perseus: @_";
			return;
		}
	}
}
			
sub _perseus_lookup_tt {
	my $self = shift;
	my( $orig, $pos, $lemma ) = split( /\t/, $_[0] );
	$self->_morph_connect();
	return unless $self::dbhandle;
	# Discard results that don't match the lemma, unless lemma is unknown
	my $lookupopts = {};
	unless( $lemma eq '<unknown>' || $lemma =~ /^\W+$/ ) {
		my %lems;
		map { $lems{$_} = 1; $lems{lc($_)} = 1 } split( /\|/, $lemma );
		$lookupopts->{'lemma'} = [ keys %lems ];
	}
	$lookupopts->{'ttpos'} = $pos if $pos;
	
	my $result = $self::dbhandle->lexicon_lookup( $orig, $lookupopts );
	# unless( !keys( %$lookupopts ) ||  $result->{'filtered'} ) {
	# 	warn "Filter on $pos / $lemma returned no results; using all results";
	# }
	my @ret = @{$result->{'objects'}};
	my %unique_wordforms;
	foreach my $obj ( @ret ) {
		my $wf = $self->_wordform_from_row( $obj );
		$unique_wordforms{$wf->to_string} = $wf;
	}
	return values( %unique_wordforms );
}

sub _perseus_lookup_str {
	my $self = shift;
	my ( $orig, $pos, $lemma ) = split( /\t/, $_[0] );
	$self->_morph_connect();
	return unless $self::dbhandle;
	# Simple morph DB lookup, and return the results. Disregard the treetagger.
	my $result = $self::dbhandle->lexicon_lookup( $orig );
	return map { $self->_wordform_from_row( $_ ) } @{$result->{'objects'}};
}
	
sub _wordform_from_row {
	my( $self, $rowobj ) = @_;
	my $lemma = $rowobj->lemma;
	$lemma =~ s/^(\D+)\d*$/$1/;
	my $wf = Text::Tradition::Collation::Reading::WordForm->new(
		'language' => $self->_get_lang(),
		'lemma' => $lemma,
		'morphology' => $rowobj->morphology,
		);
	return $wf;
}
	
1;
