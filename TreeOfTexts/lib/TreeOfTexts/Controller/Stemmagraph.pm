package TreeOfTexts::Controller::Stemmagraph;
use Moose;
use namespace::autoclean;
use Text::Tradition::Collation;
use Text::Tradition::Stemma;

BEGIN { extends 'Catalyst::Controller' }

#
# Sets the actions in this controller to be registered with no prefix
# so they function identically to actions created in MyApp.pm
#
__PACKAGE__->config(namespace => '');

=head1 NAME

TreeOfTexts::Controller::Root - Root Controller for TreeOfTexts

=head1 DESCRIPTION

[enter your description here]

=head1 METHODS

sub get_graph :Local {
	my( $self, $c ) = @_;
	# If called interactively, we have params 'display', 'output', 'witnesses'
	# If called non-interactively, we look at headers and content.
	# The body is actually a File::Temp object; this is undocumented but 
	# so it seems to be.
	my $dot_fh = $c->request->body;
	my $format = 'svg';

    # Render the dot in the given format.
    my $collation = Text::Tradition::Collation->new();
    my $stemma = Text::Tradition::Stemma->new( 'collation' => $collation, 'dot' => $dot_fh );
    $c->stash->{result} = $stemma->as_svg;
    $c->forward( "View::SVG" );
}

=head2 default

Standard 404 error page

=cut

sub default :Path {
    my ( $self, $c ) = @_;
    $c->response->body( 'Page not found' );
    $c->response->status(404);
}

=head2 end

Attempt to render a view, if needed.

=cut

sub end : ActionClass('RenderView') {}

=head1 AUTHOR

Tara L Andrews

=head1 LICENSE

This library is free software. You can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

__PACKAGE__->meta->make_immutable;

1;
