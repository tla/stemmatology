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

The root page (/).  Lists the traditions available in the DB to work on,
and should also eventually have an 'Upload new' interface.

=cut

# Just render the container page.
sub index :Path :Args(0) {
    my ( $self, $c ) = @_;
    $c->stash->{template} = 'os_container.tt';
}

## OpenSocial gadget URL calls

# Render the template for the text index gadget.
sub os_index :Local {
    my( $self, $c ) = @_;
    my $m = $c->model('Analysis');
    my @all_texts = map { $_->{'title'} } @{$m->{'data'}};
    my $m = $c->model('Directory');
    my @all_texts;
    foreach my $id ( $m->tradition_ids ) {
    	my $data = { 
    		'id' => $id,
    		'name' => $m->name( $id ),
    	};
    	push( @all_texts, $data );
    }
    
    $c->stash->{texts} = \@all_texts;
    $c->stash->{template} = 'index_gadget.tt';    
}

=head2 tradition (TODO)

The main page for a tradition, with information about it and links to the
available tools.

=head2 relationships

The relationship editor tool.

=cut

sub relationships :Local {
	my( $self, $c ) = @_;
	my $m = $c->model('Directory');
	my $tradition = $m->tradition( $c->request->params->{'textid'} );
	$c->stash->{alignment} = $tradition->collation->make_alignment_table( 'refs' );
	$c->stash->{template} = 'relationships.tt';	
}

=head2 stexaminer

The stemma analysis tool with the pretty colored table.

=cut

sub stexaminer :Local {
    my( $self, $c ) = @_;
    my $m = $c->model('Directory');
	my $tradition = $m->tradition( $c->request->params->{'textid'} );
	my $stemma = $tradition->stemma;
	# TODO Think about caching the stemma in a session 
	$c->stash->{svg} = $stemma->as_svg;
	$c->stash->{text_title} = $tradition->name;
	$c->stash->{template} = 'index.tt'; 
	# TODO Run the analysis as AJAX from the loaded page.
	my $t = run_analysis( $tradition );
	$c->stash->{variants} = $t->{'variants'};
	$c->stash->{total} = $t->{'variant_count'};
	$c->stash->{genealogical} = $t->{'genealogical_count'};
	$c->stash->{conflict} = $t->{'conflict_count'};
}

=head1 OPENSOCIAL URLs

=head2 view_table

Simple gadget to return the analysis table for the stexaminer

=cut

sub view_table :Local {
    my( $self, $c ) = @_;
    my $m = $c->model('Directory');
	my $id = $c->request->params->{'textid'};
	my $t = run_analysis( $m->tradition( $id ), $m->stemma( $id ) );
   	$c->stash->{variants} = $t->{'variants'};
    $c->stash->{template} = 'table_service.tt';
}

=head2 view_svg

Simple gadget to return the SVG for a given stemma

=cut

sub view_svg :Local {
    my( $self, $c ) = @_;
    my $m = $c->model('Directory');
    my $stemma = $m->tradition( $c->request->params->{'textid'} )->stemma;
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
