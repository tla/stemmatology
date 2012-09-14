package Text::Tradition::Language;

use strict;
use warnings;
use Module::Load;
use Moose::Role;

requires 'throw';

=head1 NAME

Text::Tradition::Language - add-on role to enable language awareness and 
morphology functions to a Text::Tradition object.  See also 
L<Text::Tradition::Morphology> for individual reading morphologies.

=head1 METHODS

=head2 language

Accessor for the primary language of the tradition. Must correspond to one
of the Text::Tradition::Language::* modules in this package.

=cut

has 'language' => (
	is => 'rw',
	isa => 'Str',
	predicate => 'has_language',
	);
	
before 'language' => sub {
	my $self = shift;
	if( @_ && $_[0] ne 'Default' ) {
		# We are trying to set the language; check that the corresponding
		# module exists.
		eval "require Text::Tradition::Language::".$_[0];
		if( $@ ) {
			throw( "Cannot load language module for @_: $@" );
		}
	}
};
    
=head2 lemmatize

Calls the appropriate lemmatization function for the language of the
tradition.

=cut

sub lemmatize {
	my $self = shift;
	unless( $self->has_language ) {
		throw( "Please set a language to lemmatize a tradition" );
	}
	my $mod = "Text::Tradition::Language::" . $self->language;
	load( $mod );
	$mod->can( 'lemmatize' )->( $self );
}

1;

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews E<lt>aurum@cpan.orgE<gt>
