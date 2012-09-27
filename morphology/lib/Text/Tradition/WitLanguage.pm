package Text::Tradition::WitLanguage;

use strict;
use warnings;
use Module::Load;
use Moose::Role;
use TryCatch;

=head1 NAME

Text::Tradition::WitLanguage - add-on role to enable language awareness and 
morphology functions to a Text::Tradition::Witness object.  Please see
L<Text::Tradition::Morphology> for more information on the morphology 
add-on distribution.

=head1 METHODS

=head2 language

Accessor for the primary language of the tradition. Must correspond to one
of the Text::Tradition::Language::* modules in this package. Used for JSON
export of a language-regularized witness text.

=begin testing

use Test::Warn;
use TryCatch;
use_ok( 'Text::Tradition' ); # with Language
use_ok( 'Text::Tradition::Witness' ); # with WitLanguage

=end testing

=cut

has 'language' => (
	is => 'rw',
	isa => 'Str',
	predicate => 'has_language',
	);
	
around 'language' => sub {
	my $orig = shift;
	my $self = shift;
	if( @_ && $_[0] ne 'Default' ) {
		# We are trying to set the language; check that the corresponding
		# module exists.
		try {
			load( "Text::Tradition::Language::".$_[0] );
		} catch ( $e ) {
			print STDERR "No language module defined for @_\n";
		}
	} elsif( !$self->has_language && $self->tradition->has_language ) {
		return $self->tradition->language;
	}
	$self->$orig( @_ );
};
    
around 'export_as_json' => sub {
	my $orig = shift;
	my $self = shift;
	my $answer = $self->$orig( @_ );
	if( $self->has_language || $self->tradition->has_language ) {
		# If we do have a language, regularize the tokens in $answer.
		my $mod = 'Text::Tradition::Language::' . $self->language;
		eval { load( $mod ); };
		# If a module doesn't exist for our language, use the base routine
		$mod = 'Text::Tradition::Language::Base' if $@;
		my $rsub = $mod->can( 'collation_normalize' ) || $mod->can( 'regularize' );
		map { $_->{'n'} = $rsub->( $_->{'t'} ) } @{$answer->{tokens}};
		if( exists $answer->{layertokens} ) {
			map { $_->{'n'} = $rsub->( $_->{'t'} ) } @{$answer->{layertokens}};
		}
	} 
	return $answer;
};

1;

