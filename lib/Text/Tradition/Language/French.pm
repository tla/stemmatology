package Text::Tradition::Language::French;

use Encode qw/ encode_utf8 decode_utf8 /;
use IPC::Run qw/ run /;
use Lingua::TagSet::Multext;
use Lingua::TagSet::TreeTagger;
use Module::Load;
use Text::Tradition::Collation::Reading::Lexeme;
use Text::Tradition::Collation::Reading::WordForm;
use TryCatch;

my $MORPHDIR = '/Users/tla/Projects/morphology';

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
	my $mr = $tf->collation->reading('99,2');
	is( $mr->text, 'minspire', "Picked correct test reading" );
	is( $mr->language, 'French', "Reading has correct language setting" );
	$mr->normal_form( "m'inspire" );
	$mr->lemmatize;
	is( $mr->lexemes, 2, "Got two lexemes for new m'inspire reading" );
}

=end testing

=cut

sub lemmatize {
	my $tradition = shift;

	# Given a tradition, lemmatize it witness by witness and see what we get.
	my $workdir = File::Temp->newdir();
	my $c = $tradition->collation;
	# First, clear out all existing lexemes from the readings. Save the
	# path as long as we went to the trouble of generating it.
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
	
	foreach my $sig ( keys %witness_paths ) {
		# Get the text as a sequence of readings and as a string
		print STDERR "Morphologizing witness $sig\n";
		_lemmatize_sequence( undef, @{$witness_paths{$sig}} );
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
						print STDERR "Adding form " . $form->to_string . 
							" to lexeme " . $nl->string . " at $reading\n";
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

=head2 reading_lookup( $rdg[, $rdg, ...] )

Looks up one or more readings using the Flemm package, and returns the
possible results.  This uses the same logic as L<lemmatize> above for the
entire tradition, but can also be used to (re-)analyze individual readings.

=cut

sub reading_lookup {
	return _lemmatize_sequence( 1, @_ );
}

sub _lemmatize_sequence {
	my( $replace, @path ) = @_;
	$DB::single = 1 if $replace;
	my $tagresult = _treetag_string( _text_from_path( 1, @path ) );
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
			my @forms = _parse_wordform( _flemm_lookup( $tag ) );
			my $lexobj = Text::Tradition::Collation::Reading::Lexeme->new(
				'string' => $lexeme, 'language' => 'French',
				'wordform_matchlist' => \@forms );
			# Find the next non-meta reading
			while( $curr_rdg && $curr_rdg->is_meta ) {
				$curr_rdg = shift @path;
			}
			unless( $curr_rdg ) {
				warn "Ran out of readings in sequence for " . $wit->sigil
					. " at $lexeme";
				last;
			}
			my $curr_rdg_text = $curr_rdg->has_normal_form 
				? $curr_rdg->normal_form : $curr_rdg->text;
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
					my $nrtext = $nr->has_normal_form ? $nr->normal_form : $nr->text;
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
				_update_reading_lexemes( $replace, $curr_rdg, @curr_lexemes );
				$curr_rdg = shift @path;
				@curr_lexemes = ();
			}
		}
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
		$pathtext .= ( $normalize && $r->has_normal_form ) 
			? $r->normal_form : $r->text;
		$last = $r;
	}
	return $pathtext;
}

# Utility function that actually calls the tree tagger.
sub _treetag_string {
	my( $text ) = @_;
	my $wittext = encode_utf8( $text );
	# Then see if we have TreeTagger
	my $taggercmd = "$MORPHDIR/TreeTagger/cmd/tree-tagger-french-utf8";
	unless( -f $taggercmd ) {
		warn "Cannot do French word lemmatization without TreeTagger";
		return;
	}
	# OK, we can run it then.
	my @cmd = ( $taggercmd );
	my( $tagresult, $err ); # Capture the output and error
	run( \@cmd, \$wittext, \$tagresult, \$err );
	# TODO check for error
	return decode_utf8( $tagresult );
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
			$morphobj = Lingua::TagSet::TreeTagger->tag2structure( $pos );
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
