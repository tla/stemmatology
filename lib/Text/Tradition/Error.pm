package Text::Tradition::Error;

use strict;
use warnings;
use Moose;
use overload '""' => \&_stringify, 'fallback' => 1;

with qw/ Throwable::X /;
use Throwable::X -all;

sub _stringify {
	my $self = shift;
	return "Error: " . $self->ident . " // " . $self->message;
}

no Moose;
__PACKAGE__->meta->make_immutable;

=head1 NAME

Text::Tradition::Error - throwable error class for CollateX package

=head1 DESCRIPTION

A basic exception class to throw around, as it were.

=cut

=head1 AUTHOR

Tara L Andrews E<lt>aurum@cpan.orgE<gt>
