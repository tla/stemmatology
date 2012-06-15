package Text::Tradition::Collation::Reading::WordForm;

use Lingua::Features::Structure;
use JSON ();
use Moose;
use Text::Tradition::Error;
use TryCatch;

=head1 NAME

Text::Tradition::Collation::Reading::WordForm - represents a
language/lemma/morphology triplet that can be associated with a Reading.

=head1 DESCRIPTION

Text::Tradition is a library for representation and analysis of collated
texts, particularly medieval ones.  A word form is used for the analysis of
Reading objects; it consists of a lemma, a language, and a code to
represent its part of speech.  In general the word forms for a particular
language should be read from / written to some morphological database.

=head1 METHODS

=head2 new

Creates a new word form from the passed options.

=head2 language

Returns the language to which this word form belongs.

=head2 lemma

Returns the lemma for the word form.

=head2 morphology

Returns an array representing this word's morphology. The contents of the
array depend on the language being used.

=cut

has 'language' => (
	is => 'ro',
	isa => 'Str',
	required => 1,
	);
	
has 'lemma' => (
	is => 'ro',
	isa => 'Str',
	required => 1,
	);
	
has 'morphstr' => (
	is => 'ro',
	isa => 'Str',
	required => 1,
	);
	
around BUILDARGS => sub {
	my $orig = shift;
	my $class = shift;
	my $args = @_ == 1 ? $_[0] : { @_ };
	if( exists $args->{'JSON'} ) {
		my @data = split( / \/\/ /, $args->{'JSON'} );
		# print STDERR "Attempting to parse " . $data[2] . " into structure";
		$args = { 'language' => $data[0], 'lemma' => $data[1],
			'morphstr' => $data[2] };
	} elsif( exists $args->{'morphology'} ) {
		# Backwards compat
		my $mobj = delete $args->{'morphology'};
		$args->{'morphstr'} = $mobj->to_string()
			if ref $mobj;
	}
	$class->$orig( $args );
};

=head2 morphology

Returns a Lingua::Features::Structure object that corresponds to morphstr.

=cut

sub morphology {
	my $self = shift;
	return unless $self->morphstr;
	my $struct;
	try {
		$struct = Lingua::Features::Structure->from_string( $self->morphstr );
	} catch {
		throw( "Morphology string " . $self->morphstr . " does not parse" );
	}
	return $struct;
}
	
=head2 to_string

Returns a string combination of language/lemma/morphology that can be used
in equivalence testing.

=cut

sub to_string {
	my $self = shift;
	return JSON->new->convert_blessed(1)->encode( $self );
}

# Rather than spitting it out as a JSON hash, encode it as a string so that
# the XML serialization doesn't become insane.
sub TO_JSON {
	my $self = shift;
	return sprintf( "%s // %s // %s", $self->language, $self->lemma,
		$self->morphstr );
}
	
sub throw {
	Text::Tradition::Error->throw( 
		'ident' => 'Wordform error',
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
