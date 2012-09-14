package Text::Tradition::Morphology;

use strict;
use warnings;
use JSON qw/ from_json /;
use Moose::Role;
use Module::Load;
use Text::Tradition::Collation::Reading::Lexeme;

use vars qw/ $VERSION /;
$VERSION = "0.1";

=head1 NAME

Text::Tradition::Morphology - morphology plugin for Text::Tradition

=head1 DESCRIPTION

The Text::Tradition::Morphology package enables lemma and part-of-speech
information for traditions and their Reading objects. This distribution
includes the L<Text::Tradition::Language> role for Traditions, the
L<Text::Tradition::Morphology> role (this package) for Readings, and a set
of Language::* modules for language-specific lemmatization.

See L<Text::Tradition::Collation::Reading::Lexeme> for more about the 
morphology object structure.

=cut

requires 'is_identical', 'is_combinable', '_combine';

has 'grammar_invalid' => (
	is => 'rw',
	isa => 'Bool',
	default => undef,
	);
	
has 'is_nonsense' => (
	is => 'rw',
	isa => 'Bool',
	default => undef,
	);

has 'normal_form' => (
	is => 'rw',
	isa => 'Str',
	predicate => '_has_normal_form',
	clearer => '_clear_normal_form',
	);

# Holds the lexemes for the reading.
has 'reading_lexemes' => (
	traits => ['Array'],
	isa => 'ArrayRef[Text::Tradition::Collation::Reading::Lexeme]',
	handles => {
		lexeme => 'get',
		lexemes => 'elements',
		has_lexemes => 'count',
		clear_lexemes => 'clear',
		add_lexeme => 'push',
		},
	default => sub { [] },
	);
	


# Make normal_form default to text, transparently.
around 'normal_form' => sub {
	my $orig = shift;
	my $self = shift;
	my( $arg ) = @_;
	if( $arg && $arg eq $self->text ) {
		$self->_clear_normal_form;
		return $arg;
	} elsif( !$arg && !$self->_has_normal_form ) {
		return $self->text;
	} else {
		$self->$orig( @_ );
	}
};

=head1 READING METHODS

Methods for the morphological information (if any) attached to readings.
A reading may be made up of multiple lexemes; the concatenated lexeme
strings ought to match the reading's normalized form.
 
See L<Text::Tradition::Collation::Reading::Lexeme> for more information
on Lexeme objects and their attributes.

=head2 has_lexemes

Returns a true value if the reading has any attached lexemes.

=head2 lexemes

Returns the Lexeme objects (if any) attached to the reading.

=head2 clear_lexemes

Wipes any associated Lexeme objects out of the reading.

=head2 add_lexeme( $lexobj )

Adds the Lexeme in $lexobj to the list of lexemes.

=head2 lemmatize

If the language of the reading is set, this method will use the appropriate
Language model to determine the lexemes that belong to this reading.  See
L<Text::Tradition::Language::lemmatize> if you wish to lemmatize an entire tradition.

=cut

sub lemmatize {
	my $self = shift;
	unless( $self->has_language ) {
		warn "Please set a language to lemmatize a tradition";
		return;
	}
	my $mod = "Text::Tradition::Language::" . $self->language;
	load( $mod );
	$mod->can( 'reading_lookup' )->( $self );

}

# For graph serialization. Return a JSON representation of the associated
# reading lexemes.
sub _serialize_lexemes {
	my $self = shift;
	my $json = JSON->new->allow_blessed(1)->convert_blessed(1);
	return $json->encode( [ $self->lexemes ] );
}

# Given a JSON representation of the lexemes, instantiate them and add
# them to the reading.
sub _deserialize_lexemes {
	my( $self, $json ) = @_;
	my $data = from_json( $json );
	return unless @$data;
	
	my @lexemes;
	foreach my $lexhash ( @$data ) {
		push( @lexemes, Text::Tradition::Collation::Reading::Lexeme->new(
			'JSON' => $lexhash ) );
	}
	$self->clear_lexemes;
	$self->add_lexeme( @lexemes );
}

sub disambiguated {
	my $self = shift;
	return 0 unless $self->has_lexemes;
	return !grep { !$_->is_disambiguated } $self->lexemes;
}

sub filter_lexemes {
	my $self = shift;
	# While we are here, get rid of any extra wordforms from a disambiguated
	# reading.
	if( $self->disambiguated ) {
		foreach my $lex ( $self->lexemes ) {
			$lex->clear_matching_forms();
			$lex->add_matching_form( $lex->form );
		}
	}
}

around 'is_identical' => sub {
	my $orig = shift;
	my $self = shift;
	my $other = shift;
	# If the base class returns true, do an extra check to make sure the
	# lexemes also match.
	my $answer = $self->$orig( $other );
	if( $answer ) {
		if( $self->disambiguated && $other->disambiguated ) {
			my $rform = join( '//', map { $_->form->to_string } $self->lexemes );
			my $uform = join( '//', map { $_->form->to_string } $other->lexemes );
			$answer = undef unless $rform eq $uform;
		} elsif( $self->disambiguated xor $other->disambiguated ) {
			$answer = undef;
		}
	}
	return $answer;
};

around 'is_combinable' => sub {
	my $orig = shift;
	my $self = shift;
	# If the reading is marked with invalid grammar or as a nonsense reading,
	# it is no longer combinable.
	return undef if $self->grammar_invalid || $self->is_nonsense;
	return $self->$orig();
};

after '_combine' => sub {
	my $self = shift;
	my $other = shift;
	my $joinstr = shift;
	$self->normal_form( 
		join( $joinstr, $self->normal_form, $other->normal_form ) );
	# Combine the lexemes present in the readings
	if( $self->has_lexemes && $other->has_lexemes ) {
		$self->add_lexeme( $other->lexemes );
	}
};

1;

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews E<lt>aurum@cpan.orgE<gt>
