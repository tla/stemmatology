package TreeOfTexts::Controller::Stemmagraph;
use Moose;
use namespace::autoclean;
use File::Temp;
use JSON;
use Text::Tradition::Collation;
use Text::Tradition::StemmaUtil qw/ phylip_pars_input /;

BEGIN { extends 'Catalyst::Controller' }

#
# Sets the actions in this controller to be registered with no prefix
# so they function identically to actions created in MyApp.pm
#
__PACKAGE__->config(namespace => '');

=head1 NAME

TreeOfTexts::Controller::Stemmagraph - Simple controller for stemma display

=head1 DESCRIPTION

[enter your description here]

=head1 METHODS

=cut

sub get_graph :Local {
	my( $self, $c ) = @_;
	# If called interactively, we have params 'display', 'output', 'witnesses'
	# If called non-interactively, we look at headers and content.
	# The body is actually a File::Temp object; this is undocumented but 
	# so it seems to be.
	my $dotfile;
	my $must_unlink = 0;
	if( $c->request->params->{'dot'} ) {
	    # Make a File::Temp object.
	    my $tmpfile = File::Temp->new( UNLINK => 0 );
	    print $tmpfile $c->request->params->{'dot'};
	    $dotfile = $tmpfile->filename;
	    $must_unlink = 1;
	} else {
	    $dotfile = $c->request->body;
	}
	my $format = 'svg';

    # Render the dot in the given format.
    my $collation = Text::Tradition::Collation->new();
    my $stemma = Text::Tradition::Stemma->new( 'collation' => $collation, 'dot' => $dotfile );
    unlink( $dotfile ) if $must_unlink;
    $c->stash->{result} = $stemma->as_svg;
    $c->forward( "View::SVG" );
}

=head2 character_matrix

Given an alignment table in JSON form, in the parameter 'alignment', returns a
character matrix suitable for input to Phylip PARS. 

=cut

sub character_matrix :Local {
	my( $self, $c ) = @_;
	my $json = $c->request->params->{'alignment'};
	$c->log->debug( $json );
	my $table = from_json( $json );
	my $matrix = phylip_pars_input( $table );
	$c->stash->{'result'} = { 'matrix' => $matrix };
	$c->forward( 'View::JSON' );
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
