package Text::Tradition::Collation::Reading::Lexeme;

use Moose;
use JSON ();
use Module::Load;
use Text::Tradition::Collation::Reading::WordForm;
use Text::Tradition::Error;

=head1 NAME

Text::Tradition::Collation::Reading::Lexeme - represents the components of
a Reading.

=head1 DESCRIPTION

Text::Tradition is a library for representation and analysis of collated
texts, particularly medieval ones.  A word form is used for the analysis of
Reading objects; it consists of a lemma, a language, and a code to
represent its part of speech.  In general the word forms for a particular
language should be read from / written to some morphological database.

=head1 METHODS

=head2 new

Creates a new lexeme from the passed options.

=head2 language

Returns the language to which this lexeme belongs.

=head2 normalized

Returns the canonical string version of this lexeme.

=head2 matches

Returns the number of possible word forms for this lexeme, as drawn from
the appropriate database.

=head2 matching_forms

Returns an array of the possible word forms for this lexeme.

=head2 matching_form( $index )

Returns the form at $index in the list of matching forms.

=head2 is_disambiguated

Returns true if a single wordform has been picked as 'correct' for this
lexeme in its context.

=head2 form

Returns the correct word form (if any has been selected) for the lexeme in
its context.

=cut

# TODO need to be able to populate this from DB
has 'language' => (
	is => 'ro',
	isa => 'Str',
	required => 1,
	);
	
has 'string' => (
	is => 'rw',
	isa => 'Str',
	required => 1,
	);

has 'wordform_matchlist' => (
	isa => 'ArrayRef[Text::Tradition::Collation::Reading::WordForm]',
	traits => ['Array'],
	handles => {
		'matches' => 'count',
		'matching_forms' => 'elements',
		'matching_form' => 'get',
		'add_matching_form' => 'push',
		},
	default => sub { [] },
	);

has 'is_disambiguated' => (
	is => 'rw',
	isa => 'Bool',
	default => undef,
	);
	
has 'form' => (
	is => 'ro',
	isa => 'Text::Tradition::Collation::Reading::WordForm',
	writer => '_set_form',
	);
	
around BUILDARGS => sub {
	my $orig = shift;
	my $class = shift;
	my $args = @_ == 1 ? $_[0] : { @_ };
	if( exists $args->{JSON} ) {
		my $data = $args->{JSON};
		if( exists $data->{'form'} && $data->{'form'} ) {
			my $form = Text::Tradition::Collation::Reading::WordForm->new(
				'JSON' => $data->{'form'} );
			$data->{'form'} = $form;
		}
		if( exists $data->{'wordform_matchlist'} && $data->{'wordform_matchlist'} ) {
			my @ml;
			foreach my $wfjson ( @{$data->{'wordform_matchlist'}} ) {
				push( @ml, Text::Tradition::Collation::Reading::WordForm->new(
					'JSON' => $wfjson ) );
			}
			$data->{'wordform_matchlist'} = \@ml;
		}
		$args = $data;
	}
	$class->$orig( $args );
};
	
# Do auto-disambiguation if we were created with a single wordform
sub BUILD {
	my $self = shift;

	if( $self->matches == 1 ) {
		$self->disambiguate( 0 );
	}	
}

around 'add_matching_form' => sub {
	my $orig = shift;
	my $self = shift;
	my @realargs;
	foreach my $a ( @_ ) {
		if( ref( $a ) ) {
			push( @realargs, $a );
		} else {
			# Make the wordform from the string
			my $wf = Text::Tradition::Collation::Reading::WordForm->new(
				'JSON' => $a );
			push( @realargs, $wf );
		}
	}
	return $self->$orig( @realargs );
};

=head2 disambiguate( $index )

Selects the word form at $index in the list of matching forms, and asserts
that this is the correct form for the lexeme.

=cut

sub disambiguate {
	my( $self, $idx ) = @_;
	my $form = $self->matching_form( $idx );
	throw( "There is no candidate wordform at index $idx" )
		unless $form;
	$self->_set_form( $form );
	$self->is_disambiguated( 1 );	
}

=head2 has_form( $rep ) 

Returns the index of the matching form whose string representation is in $rep, 
or else undef if none is found.

=cut

sub has_form {
	my( $self, $rep ) = @_;
	my $i = 0;
	foreach my $mf ( $self->matching_forms ) {
		my $struct = $mf->TO_JSON;
		return $i if $struct eq $rep;
		$i++;
	}
	return undef;
}
		

sub TO_JSON {
	my $self = shift;
	my $hash = {};
	# Do the scalar keys
	map { $hash->{$_} = $self->$_ if defined $self->$_ } 
		qw/ language string is_disambiguated form /; 
	$hash->{'wordform_matchlist'} = [ $self->matching_forms ] if $self->matches;
	return $hash;
}

sub throw {
	Text::Tradition::Error->throw( 
		'ident' => 'Lexeme error',
		'message' => $_[0],
		);
}

no Moose;
__PACKAGE__->meta->make_immutable;

1;

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews E<lt>aurum@cpan.orgE<gt>
