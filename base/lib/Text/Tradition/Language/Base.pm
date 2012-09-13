package Text::Tradition::Language::Base;

use strict;
use warnings;
use Encode qw/ encode_utf8 decode_utf8 /;
use Exporter 'import';
use vars qw/ @EXPORT_OK /;
use IPC::Run qw/ run /;
use Module::Load;
use Text::Tradition::Collation::Reading::Lexeme;
use Text::Tradition::Collation::Reading::WordForm;
use TryCatch;

@EXPORT_OK = qw/ lemmatize_treetagger reading_lookup_treetagger lfs_morph_tags /;

=head1 NAME

Text::Tradition::Language::Base - Base subroutines for lemmatization of words

=head1 DESCRIPTION

Common routines for applying morphological tagging to a Text::Tradition. Used
with callbacks from the named language packages.

=head1 SUBROUTINES

=head2 lemmatize_treetagger( $tradition )

Evaluates the tradition with the given options, and returns the results.

=cut

sub lemmatize_treetagger {
	my( $tradition, %opts ) = @_;

	# Given a tradition, lemmatize it witness by witness and see what we get.
	my $c = $tradition->collation;
	# First, clear out all existing lexemes from the readings. 
	my %witness_paths = _clear_reading_lexemes( $tradition );
	
	foreach my $sig ( keys %witness_paths ) {
		# Get the text as a sequence of readings and as a string
		my %witopts = (
			'path' => $witness_paths{$sig},
			%opts
			);
		_lemmatize_treetagger_sequence( %witopts );
	}
}

sub _clear_reading_lexemes {
	my $tradition = shift;
		my $c = $tradition->collation;
	# Clear out all existing lexemes from the readings. Save the path as long 
	# as we went to the trouble of generating it.
	my %witness_paths;
	foreach my $wit ( $tradition->witnesses ) {
		my @sigla = ( $wit->sigil );
		push( @sigla, $wit->sigil . $c->ac_label ) if $wit->is_layered;
		foreach my $sig ( @sigla ) {
			my @path = grep { !$_->is_meta } 
				$c->reading_sequence( $c->start, $c->end, $sig );
			map { $_->clear_lexemes } @path;
			$witness_paths{$sig} = \@path;
		}
	}
	return %witness_paths;
}

=head2 reading_lookup( $rdg[, $rdg, ...] )

Looks up one or more readings using the Flemm package, and returns the
possible results.  This uses the same logic as L<lemmatize> above for the
entire tradition, but can also be used to (re-)analyze individual readings.

=cut

sub reading_lookup_treetagger {
	my %opts = @_;
	$opts{'replace'} = 1;
	return _lemmatize_treetagger_sequence( %opts );
}

sub _lemmatize_treetagger_sequence {
	my %opts = @_;
	my @path = @{$opts{'path'}};
	my $tagresult = _treetag_string( _text_from_path( 1, @path ), $opts{'language'} );
	if( $tagresult ) {
		# Map the tagged words onto the original readings, splitting 
		# them up into lexemes where necessary.
		# NOTE we can have multiple lexemes in a reading, but not
		# multiple readings to a lexeme.
		my @tags = split( /\n/, $tagresult );
		my @lexemes;
		my $curr_rdg = shift @path;
		my @curr_lexemes;
		my $unused_rdg_part;
		foreach my $tag ( @tags ) {
			# Get the original word
			my( $lexeme, @rest ) = split( /\t/, $tag );
			# Lemmatize the whole
			# TODO error trap this
			my @forms = $opts{'callback'}( $tag );

			my $lexobj = Text::Tradition::Collation::Reading::Lexeme->new(
				'string' => $lexeme, 'language' => $opts{'language'},
				'wordform_matchlist' => \@forms );
			# Find the next non-meta reading
			while( $curr_rdg && $curr_rdg->is_meta ) {
				$curr_rdg = shift @path;
			}
			unless( $curr_rdg ) {
				warn "Ran out of readings in sequence at $lexeme";
				last;
			}
			my $curr_rdg_text = $curr_rdg->normal_form;
			if( $unused_rdg_part &&
				$unused_rdg_part =~ /^\Q$lexeme\E(\s*)(.*)$/ ) {
				# Nth part of curr_rdg
				$unused_rdg_part = $2;
				push( @curr_lexemes, $lexobj );
			} elsif( $curr_rdg_text =~ /^\Q$lexeme\E(\s*)(.*)$/ ) {
				# Flag an error if there is already an unused reading part.
				warn "Skipped over unused text $unused_rdg_part at $curr_rdg"
					if $unused_rdg_part;
				$unused_rdg_part = $2; # will be empty if the whole reading matched
				push( @curr_lexemes, $lexobj );
			} else {
				# We do not cope with the idea of a lexeme being 
				# spread across multiple readings.
				warn "Word sequence changed unexpectedly in text";
				# See if we can find a matching reading
				my @lookahead;
				my $matched;
				while( my $nr = shift @path ) {
					my $nrtext = $nr->normal_form;
					if( $nrtext =~ /^\Q$lexeme\E/ ) {
						$curr_rdg = $lookahead[-1] if @lookahead;
						$matched = 1;
						last;
					} else {
						push( @lookahead, $nr );
					}
				}
				# No match? Restore the state we had
				unless( $matched ) {
					unshift( @path, @lookahead );
				}
				# Trigger a move
				$unused_rdg_part = '';
			}
			
			unless( $unused_rdg_part ) {
				# Record the lexemes for the given reading.
				#print STDERR sprintf( "Adding lexeme(s) %s to reading %s (%s)\n",
				#	join( ' ', map { $_->string } @curr_lexemes ),
				#	$curr_rdg->id, $curr_rdg->text );
				_update_reading_lexemes( $opts{replace}, $curr_rdg, @curr_lexemes );
				$curr_rdg = shift @path;
				@curr_lexemes = ();
			}
		}
	}
}

sub _update_reading_lexemes {
	my( $replace, $reading, @lexemes ) = @_;
	if( $reading->has_lexemes && !$replace ) {
		# We need to merge what is in @lexemes with what we have already.
		my @oldlex = $reading->lexemes;
		my $cmp1 = join( '||', map { $_->string } @oldlex );
		my $cmp2 = join( '||', map { $_->string } @lexemes );
		if ( @oldlex == @lexemes && $cmp1 eq $cmp2 ) {
			# The lexeme strings are the same, so merge the possible
			# word forms from new to old.
			foreach my $i ( 0 .. $#lexemes ) {
				my $ol = $oldlex[$i];
				my $nl = $lexemes[$i];
				my %ofw;
				map { $ofw{$_->to_string} = 1 } $ol->matching_forms;
				foreach my $form ( $nl->matching_forms ) {
					unless( $ofw{$form->to_string} ) {
						# print STDERR "Adding form " . $form->to_string . 
						# 	" to lexeme " . $nl->string . " at $reading\n";
						$ol->add_matching_form( $form );
						$ol->is_disambiguated(0);
					}
				}
			}
		} else {
			warn "Lexeme layout for $reading changed; replacing the lot";
			$reading->clear_lexemes;
			$reading->add_lexeme( @lexemes );
		}
	} else {
		$reading->clear_lexemes if $replace;
		$reading->add_lexeme( @lexemes );
	}
}

# Utility function so that we can cheat and use it when we need both the path
# and its text.
sub _text_from_path {
	my( $normalize, @path ) = @_;
	my $pathtext = '';
	my $last;
	foreach my $r ( @path ) {
		unless ( $r->join_prior || !$last || $last->join_next ) {
			$pathtext .= ' ';
		} 
		$pathtext .= $normalize ? $r->normal_form : $r->text;
		$last = $r;
	}
	return $pathtext;
}

# Utility function that actually calls the tree tagger.
sub _treetag_string {
	my( $text, $lang ) = @_;
	my $wittext = encode_utf8( $text );
	# Then see if we have TreeTagger
	try {
		load( 'Lingua::TreeTagger' );
	} catch {
		warn "Cannot run TreeTagger without Lingua::TreeTagger module";
		return '';
	}
	# OK, we can run it then.
	# First upgrade to UTF8 for necessary languages.
	my @utf8_supported = qw/ French Latin Greek /;
	my %ttopts = ( 'language' => $lang, 'options' => [ qw/ -token -lemma / ] );
	if( grep { $_ eq $lang } @utf8_supported ) {
		$ttopts{'use_utf8'} = 1;
	}
	# Now instantiate and run the tagger.
	my $tagger = Lingua::TreeTagger->new( %ttopts );
	my $tagresult = $tagger->tag_text( \$text );
	
	# TODO maybe send the tokens back rather than the interpreted string...
	return $tagresult->as_text();
}

=head2 lfs_morph_tags

Return a data structure describing the available parts of speech and their attributes
from the Lingua::Features::Structure class currently defined.

=cut

sub lfs_morph_tags {
	load('Lingua::Features::StructureType');
	my $tagset = { 'structures' => [], 'features' => {} };
	foreach my $lfs ( sort { _by_structid( $a->id, $b->id ) } Lingua::Features::StructureType->types() ) {
		my $tsstruct = { 'id' => $lfs->id, 'desc' => $lfs->desc, 'use_features' => [] };
		foreach my $ftid ( Lingua::Features::StructureType->type($lfs->id)->features ) {
			my $ftype = $lfs->feature_type( $ftid );
			if( !$ftype && $lfs->base ) {
				$ftype = $lfs->base->feature_type( $ftid );
			}
			if( $ftype ) {
				push( @{$tsstruct->{'use_features'}}, $ftid );
				if( $ftid eq 'type' ) {
					# Type values change according to category
					$ftid .= " (" . $lfs->id . ")";
				}
				my $tfstruct = { 'id' => $ftid, 'values' => [] };
				foreach my $fval( $ftype->values ) {
					push( @{$tfstruct->{'values'}}, 
						{ 'short' => $fval, 'long' => $ftype->value_name( $fval ) } );
				}
				$tagset->{'features'}->{$ftid} = $tfstruct;
			}
		}
		push( @{$tagset->{'structures'}}, $tsstruct );
	}
	return $tagset;
}

sub _by_structid {
	my( $a, $b ) = @_;
	return -1 if $a eq 'cat';
	return 1 if $b eq 'cat';
	return $a cmp $b;
}

1;

=head2 TODO

=over

=item * Handle package dependencies more gracefully

=back

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews E<lt>aurum@cpan.orgE<gt>
