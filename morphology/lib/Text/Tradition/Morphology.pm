package Text::Tradition::Morphology;

use strict;
use warnings;
use JSON qw/ from_json /;
use Moose::Role;
use Module::Load;
use Text::Tradition::Collation::Reading::Lexeme;

use vars qw/ $VERSION /;
$VERSION = "0.2";

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

has 'language' => (
	is => 'ro',
	isa => 'Str',
	predicate => 'has_language',
	);
	
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
	


# Make normal_form default to text, transparently; also propagate the
# normal form given if the reading is a lemma.
around 'normal_form' => sub {
	my $orig = shift;
	my $self = shift;
	unless( @_ ) {
		# return the right default
		return $self->_has_normal_form ? $self->$orig() : $self->text;
	}
	# If we are setting a new normal form, we return a list of the
	# readings that were changed.
	my $arg = shift;
	my @altered = ( $self );
	if( $arg && $arg eq $self->text ) {
		$self->_clear_normal_form;
	} else {
		$self->$orig( $arg );
		push( @altered, $self->push_normal_form() ) if $self->is_lemma;
	}
	return @altered;
};

=head1 READING METHODS

Methods for the morphological information (if any) attached to readings.
A reading may be made up of multiple lexemes; the concatenated lexeme
strings ought to match the reading's normalized form.
 
See L<Text::Tradition::Collation::Reading::Lexeme> for more information
on Lexeme objects and their attributes.

=head2 has_lexemes()

Returns a true value if the reading has any attached lexemes.

=head2 lexemes()

Returns the Lexeme objects (if any) attached to the reading.

=head2 clear_lexemes()

Wipes any associated Lexeme objects out of the reading.

=head2 add_lexeme( $lexobj )

Adds the Lexeme in $lexobj to the list of lexemes.

=head2 lemmatize()

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

=head2 regularize

Call the 'regularize' function of the appropriate language model on our
own reading text. This is a rules-based function distinct from 'normal_form',
which can be set to any arbitrary string.

=cut

# TODO Test this stuff

sub regularize {
	my $self = shift;
	if( $self->has_language ) {
		# If we do have a language, regularize the tokens in $answer.
		my $mod = 'Text::Tradition::Language::' . $self->language;
		eval { load( $mod ); };
		# If a module doesn't exist for our language, use the base routine
		if( $@ ) {
			$mod = 'Text::Tradition::Language::Base';
			load( $mod );
		}
		return $mod->can( 'regularize' )->( $self->text );
	} else {
		return $self->text;
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

=head2 relationship_added( $related_reading, $relationship )

To be called when a relationship is set, to implement the consequences of 
certain relationships.

=head2 push_normal_form()

Copy the normal form of a reading to all its orthographically related readings.

=begin testing

# Test that normal form follows lemma setting. Draws on code both here and in
# the base module.

use Text::Tradition;

my $t = Text::Tradition->new(
	input => 'Self',
	file => 't/data/florilegium_graphml.xml' );
my $c = $t->collation;

# First try lemmatizing and then adding a relationship
my $r1 = $c->reading('w42');
my $r2 = $c->reading('w44');
my @changed = $r1->normal_form('FOO');
is_deeply( \@changed, [ $r1 ], "Normal form change produced no side effect" );
$r2->normal_form('BAR');

@changed = $r1->make_lemma( 1 );
is_deeply( \@changed, [ $r1 ], "Lemma flag changed only the concerned reading" );
is( $r1->normal_form, 'FOO', "nothing changed yet" );
is( $r2->normal_form, 'BAR', "nothing changed yet" );

@changed = ();
$c->add_relationship( $r1, $r2, { type => 'spelling' }, \@changed );
is_deeply( \@changed, [ $r2 ], "We were informed that reading 2 changed" );
is( $r2->normal_form, 'FOO', "Normal form followed lemma" );

# Now try setting relationships and then lemmatizing
my $r3 = $c->reading('w98');
my $r4 = $c->reading('w100');
my $r5 = $c->reading('w103');
$r3->normal_form('YAN');
$r4->normal_form('TAN');
$r5->normal_form('TETHERA');

@changed = ();
$c->add_relationship( $r3, $r4, { type => 'orthographic', propagate => 1 }, \@changed );
$c->add_relationship( $r3, $r5, { type => 'orthographic', propagate => 1 }, \@changed );
is( scalar( @changed ), 0, "No reading side effects yet" );
is( $r3->normal_form, 'YAN', "nothing changed yet" );
is( $r4->normal_form, 'TAN', "nothing changed yet" );
is( $r5->normal_form, 'TETHERA', "nothing changed yet" );

@changed = $r3->make_lemma( 1 );
my %present;
map { $present{$_->id} = 1 } @changed;
ok( $present{$r3->id}, "Informed of change to reading 3" );
ok( $present{$r4->id}, "Informed of change to reading 4" );
ok( $present{$r5->id}, "Informed of change to reading 5" );
is( scalar keys %present, 3, "Not informed of further changes" );
is( $r4->normal_form, 'YAN', "normal form propagated" );
is( $r5->normal_form, 'YAN', "normal form propagated" );

# Now try modifying the normal form and making sure the change is propagated
@changed = $r3->normal_form( 'JIGGIT' );
%present = ();
map { $present{$_->id} = 1 } @changed;
ok( $present{$r3->id}, "Informed of change to reading 3" );
ok( $present{$r4->id}, "Informed of change to reading 4" );
ok( $present{$r5->id}, "Informed of change to reading 5" );
is( scalar keys %present, 3, "Not informed of further changes" );
is( $r4->normal_form, 'JIGGIT', "new normal form propagated" );
is( $r5->normal_form, 'JIGGIT', "new normal form propagated" );

# ...and that no change is propagated if the reading isn't a lemma.
$r4->normal_form( 'JOLLY' );
is( $r3->normal_form, 'JIGGIT', "normal form on non-lemma not propagated" );
is( $r5->normal_form, 'JIGGIT', "normal form on non-lemma not propagated" );


# Finally, try a relationship that shouldn't propagate the normal form
my $r6 = $c->reading('w91');
my $r7 = $c->reading('w92');
$r6->normal_form('BAZ');
$r7->normal_form('QUUX');
$r6->make_lemma( 1 );

@changed = ();
$c->add_relationship( $r6, $r7, { type => 'grammatical' }, \@changed );
is( $r7->normal_form, 'QUUX', "normal form on grammatical relationship unchanged" );
is( scalar @changed, 0, "No readings were marked as changed" );

=end testing

=cut

sub relationship_added {
	my( $rdg1, $rdg2, $rel ) = @_;
	my $lemma = $rdg1->is_lemma ? $rdg1 : ( $rdg2->is_lemma ? $rdg2 : undef );
	my @altered_readings;
	if( $lemma ) {
		@altered_readings = $lemma->push_normal_form();
	}
	return @altered_readings;
}

sub push_normal_form {
	my $self = shift;
	# Set the normal form on all orthographically related readings to match
	# the normal form on this one.
	my @altered_readings;
	my $filter = sub { 
		my $rl = shift; 
		my $rltype = $self->collation->relations->type( $rl->type );
		return $rltype->bindlevel < 2 
	};
	foreach my $r ( $self->related_readings( $filter ) ) {
		if( $r->normal_form ne $self->normal_form ) {
			$r->normal_form( $self->normal_form );
			push( @altered_readings, $r );
		}
	}
	return @altered_readings;
}

1;

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews E<lt>aurum@cpan.orgE<gt>
