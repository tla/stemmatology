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

sub index :Path :Args(0) {
    my ( $self, $c ) = @_;

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
    $c->stash->{template} = 'frontpage.tt';
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
	my $table = $tradition->collation->make_alignment_table();
	my $witlist = map { $_->{'witness'} } @{$table->{'alignment'}};
	$c->stash->{witnesses} = $witlist;
	$c->stash->{alignment} = $table;
	$c->stash->{template} = 'relate.tt';	
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

=head2 alignment_table 

Return a JSON alignment table of a given text.

=cut

sub alignment_table :Local {
	my( $self, $c ) = @_;
	my $m = $c->model( 'Directory' );
	my $tradition = $m->tradition( $c->request->params->{'textid'} );
	my $table = $tradition->collation->make_alignment_table();
	$c->stash->{'result'} = $table;
	$c->forward-( 'View::JSON' );
}

=head1 MICROSERVICE CALLS

=head2 renderSVG

Parse the passed collation data and return an SVG of the collated text.  Takes
the following parameters:

=over 4

=item * data - The collation data itself.

=item * input - The data format.  Valid values include CollateX, Self, TEI (for parallel segmentation) eventually Tabular.

=item * name - A name for the text. Not so important for this function.

=cut

# Utility function to render SVG from a graph input.
sub renderSVG :Local {
	my( $self, $c ) = @_;
	my $format = $c->request->param('format') || 'string';
	my $type = $c->request->body_params->{'type'};
	my $name = $c->request->param('name') || 'Collation graph';
	my $data = $c->request->body_params->{'data'};
	$c->log->debug( $data );
	my $tradition = Text::Tradition->new( 
		'name' => $name,
		'input' => $type,
		$format => $data,
		);
	$c->log->debug( "Got tradition with " . $tradition->collation->readings . " readings" );
	$c->stash->{'result'} = $tradition->collation->as_svg;
	$c->forward('View::SVG');
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
    $c->stash->{template} = 'table_gadget.tt';
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
