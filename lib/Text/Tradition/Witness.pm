#!/usr/bin/env perl

package Text::Tradition::Witness;
use Moose;

has 'sigil' => (
		is => 'rw',
		isa => 'Str',
		);

has 'text' => (
	       is => 'rw',
	       isa => 'Array',
	       );

no Moose;
__PACKAGE__->meta->make_immutable;
