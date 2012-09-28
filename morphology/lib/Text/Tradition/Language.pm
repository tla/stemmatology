package Text::Tradition::Language;

use strict;
use warnings;
use Module::Load;
use Moose::Role;
use TryCatch;

requires 'throw';

=head1 NAME

Text::Tradition::Language - add-on role to enable language awareness and 
morphology functions to a Text::Tradition object.  Please see
L<Text::Tradition::Morphology> for more information on the morphology 
add-on distribution.

=head1 METHODS

=head2 language

Accessor for the primary language of the tradition. Must correspond to one
of the Text::Tradition::Language::* modules in this package.

=begin testing

use Test::Warn;
use TryCatch;
use_ok( 'Text::Tradition' ); # with Language

# Test setting and recovering language
my $t = Text::Tradition->new( input => 'Self', file => 't/data/legendfrag.xml' );
warning_like { $t->language( 'Klingon' ); } qr/^Cannot load any language/,
	"Got expected warning for setting of unsupported language";
$t->language( 'English' );
is( $t->language, 'English', "Successfully set supported language" );

# Test bad attempt to lemmatize - proper lemmatization is tested separately
my $bt = Text::Tradition->new( input => 'Self', file => 't/data/besoin.xml' );
try {
	$bt->lemmatize;
	ok( 0, "Failed to throw error on lemmatizing without language" );
} catch( Text::Tradition::Error $e ) {
	is( $e->message, "Please set a language to lemmatize a tradition",
		"Got correct error thrown for lemmatization without set language" );
} catch {
	ok( 0, "Unexpected error on bad lemmatization attempt" );
}

=end testing

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
		try {
			load( "Text::Tradition::Language::".$_[0] );
		} catch ( $e ) {
			warn "Cannot load any language module for @_";
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
		$self->throw( "Please set a language to lemmatize a tradition" );
	}
	my $mod = "Text::Tradition::Language::" . $self->language;
	try {
		load( $mod );
	} catch ( $e ) {
		$self->throw( "Cannot load language module for " . $self->language );
	}
	$self->throw( "Language module $mod has no lemmatize function" )
		unless $mod->can( 'lemmatize' );
	$mod->can( 'lemmatize' )->( $self );
}

1;

