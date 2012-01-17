package TreeOfTexts::View::TT;

use strict;
use warnings;

use base 'Catalyst::View::TT';

__PACKAGE__->config(
    TEMPLATE_EXTENSION => '.tt',
    INCLUDE_PATH => [
    	TreeOfTexts->path_to( 'root', 'src' ),
    ],
    render_die => 1,
);

=head1 NAME

TreeOfTexts::View::TT - TT View for TreeOfTexts

=head1 DESCRIPTION

TT View for TreeOfTexts.

=head1 SEE ALSO

L<TreeOfTexts>

=head1 AUTHOR

Tara L Andrews

=head1 LICENSE

This library is free software. You can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
