package TreeOfTexts::Controller::Root;
use Moose;
use namespace::autoclean;
use Text::Tradition::Analysis qw/ run_analysis /;


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

    my $m = $c->model('Directory');
    my @all_texts;
    foreach my $id ( $m->traditions ) {
    	my $data = { 
    		'id' => $id,
    		'name' => $m->tradition( $id )->name,
    		'has_stemma' => defined $m->stemma( $id ),
    	};
    	push( @all_texts, $data );
    }
    
    $c->stash->{texts} = \@all_texts;
    $c->stash->{template} = 'frontpage.tt';
}

sub relationships :Local {
	my( $self, $c ) = @_;
	my $m = $c->model('Directory');
	my $tradition = $m->tradition( $c->request->params->{'textid'} );
	$c->stash->{alignment} = $tradition->collation->make_alignment_table( 'refs' );
	$c->stash->{template} = 'relationships.tt';	
}

sub stexaminer :Local {
    my( $self, $c ) = @_;
    my $m = $c->model('Directory');
	my $id = $c->request->params->{'textid'};
	my $tradition = $m->tradition( $id );
	my $stemma = $m->stemma( $id );
	my $t = run_analysis( $tradition, $stemma );
	$c->stash->{svg} = $stemma->as_svg;
	$c->stash->{variants} = $t->{'variants'};
	$c->stash->{text_title} = $tradition->name;
	$c->stash->{total} = $t->{'variant_count'};
	$c->stash->{genealogical} = $t->{'genealogical_count'};
	$c->stash->{conflict} = $t->{'conflict_count'};
	$c->stash->{template} = 'index.tt'; 
}

sub view_table :Local {
    my( $self, $c ) = @_;
    my $m = $c->model('Directory');
	my $id = $c->request->params->{'textid'};
	my $t = run_analysis( $m->tradition( $id ), $m->stemma( $id ) );
   	$c->stash->{variants} = $t->{'variants'};
    $c->stash->{template} = 'table_gadget.tt';
}

sub view_svg :Local {
    my( $self, $c ) = @_;
    my $m = $c->model('Directory');
	my $stemma = $m->stemma( $c->request->params->{'textid'} );
	if( $stemma ) {
	   	$c->stash->{svg} = $stemma->as_svg;
	}
    $c->stash->{template} = 'stemma_gadget.tt';
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
