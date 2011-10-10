package TreeOfTexts::Controller::Root;
use Moose;
use namespace::autoclean;
use TreeOfTexts::Model::Analysis qw/ run_analysis /;

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

=head2 index

The root page (/)

=cut

sub index :Path :Args(0) {
    my ( $self, $c ) = @_;

    my $m = $c->model('Analysis');
    my $i = 0;
    my @all_texts = map { $_->{'title'} } @{$m->{'data'}};
    $c->stash->{texts} = \@all_texts;
    $c->stash->{template} = 'frontpage.tt';
}

sub view_text :Local {
    my( $self, $c ) = @_;
    my $m = $c->model('Analysis');
    my $t = $m->{'data'}->[ $c->request->params->{'textid'} ];
	$c->stash->{svg} = $t->{'svg'};
	$c->stash->{variants} = $t->{'variants'};
	$c->stash->{text_title} = $t->{'title'};
	$c->stash->{total} = $t->{'variant_count'};
	$c->stash->{genealogical} = $t->{'genealogical_count'};
	$c->stash->{conflict} = $t->{'conflict_count'};
	$c->stash->{template} = 'index.tt'; 
}

sub view_table :Local {
    my( $self, $c ) = @_;
    my $m = $c->model( 'Analysis' );
    my $t = $m->{'data'}->[ $c->request->params->{'textid'} ];
   	$c->stash->{variants} = $t->{'variants'};
    $c->stash->{template} = 'table_gadget.tt';
}

sub view_svg :Local {
    my( $self, $c ) = @_;
    my $m = $c->model( 'Analysis' );
    my $t = $m->{'data'}->[ $c->request->params->{'textid'} ];
    $c->stash->{result} = $t->{'svg'};
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
