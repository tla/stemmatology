package stemmaweb::Controller::Root;
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

stemmaweb::Controller::Root - Root Controller for stemmaweb

=head1 DESCRIPTION

Serves up the main container pages.

=head1 URLs

=head2 index

The root page (/).  Serves the main container page, from which the various
components will be loaded.

=cut

sub index :Path :Args(0) {
    my ( $self, $c ) = @_;

    $c->stash->{template} = 'index.tt';
}

=head1 Elements of index page

=head2 directory

 GET /directory

Serves a snippet of HTML that lists the available texts.  This returns texts belonging to the logged-in user if any, otherwise it returns all public texts.

=cut

sub directory :Local :Args(0) {
	my( $self, $c ) = @_;
    my $m = $c->model('Directory');
    # Is someone logged in?
    my %usertexts;
    if( $c->user_exists ) {
    	my $user = $c->user->get_object;
    	my @list = $m->traditionlist( $user );
    	map { $usertexts{$_->{id}} = 1 } @list;
		$c->stash->{usertexts} = \@list;
		$c->stash->{is_admin} = 1 if $user->is_admin;
	}
	# List public (i.e. readonly) texts separately from any user (i.e.
	# full access) texts that exist. Admin users therefore have nothing
	# in this list.
	my @plist = grep { !$usertexts{$_->{id}} } $m->traditionlist('public');
	$c->stash->{publictexts} = \@plist;
	$c->stash->{template} = 'directory.tt';
}

=head2 textinfo

 GET /textinfo/$textid
 
Returns the page element populated with information about a particular text.

=cut

sub textinfo :Local :Args(1) {
	my( $self, $c, $textid ) = @_;
	my $tradition = $c->model('Directory')->tradition( $textid );
	# Need text name, witness list, scalar readings, scalar relationships, stemmata
	my $textinfo = {
		textid => $textid,
		traditionname => $tradition->name,
		witnesses => [ map { $_->sigil } $tradition->witnesses ],
		readings => scalar $tradition->collation->readings,
		relationships => scalar $tradition->collation->relationships
	};
	my @stemmasvg = map { $_->as_svg({ size => [ 500, 375 ] }) } $tradition->stemmata;
	map { $_ =~ s/\n/ /mg } @stemmasvg;
	$textinfo->{stemmata} = \@stemmasvg;
	$c->stash->{'result'} = $textinfo;
	$c->forward('View::JSON');
}

=head2 variantgraph

 GET /variantgraph/$textid
 
Returns the variant graph for the text specified at $textid, in SVG form.

=cut

sub variantgraph :Local :Args(1) {
	my( $self, $c, $textid ) = @_;
	my $tradition = $c->model('Directory')->tradition( $textid );
	my $collation = $tradition->collation;
	$c->stash->{'result'} = $collation->as_svg;
	$c->forward('View::SVG');
}
	
=head2 alignment

 GET /alignment/$textid

Returns an alignment table for the text specified at $textid.

=cut

sub alignment :Local :Args(1) {
	my( $self, $c, $textid ) = @_;
	my $tradition = $c->model('Directory')->tradition( $textid );
	my $collation = $tradition->collation;
	my $alignment = $collation->alignment_table;
	
	# Turn the table, so that witnesses are by column and the rows
	# are by rank.
	my $wits = [ map { $_->{'witness'} } @{$alignment->{'alignment'}} ];
	my $rows;
	foreach my $i ( 0 .. $alignment->{'length'} - 1 ) {
		my @rankrdgs = map { $_->{'tokens'}->[$i]->{'t'} } 
			@{$alignment->{'alignment'}};
		push( @$rows, { 'rank' => $i+1, 'readings' => \@rankrdgs } );
	}
	$c->stash->{'witnesses'} = $wits;
	$c->stash->{'table'} = $rows;
	$c->stash->{'template'} = 'alignment.tt';
}

=head2 stemma

 GET /stemma/$textid
 POST /stemma/$textid, { 'dot' => $dot_string }

Returns an SVG representation of the stemma hypothesis for the text.  If 
the URL is called with POST and a new dot string, updates the stemma and
returns the SVG as with GET.

=cut

sub stemma :Local :Args(1) {
	my( $self, $c, $textid ) = @_;
	my $m = $c->model('Directory');
	my $tradition = $m->tradition( $textid );
	
	if( $c->req->method eq 'POST' ) {
		# Update the stemma
		my $dot = $c->request->body_params->{'dot'};
		$tradition->add_stemma( $dot );
		$m->store( $tradition );
	}
	
	$c->stash->{'result'} = $tradition->stemma_count
		? $tradition->stemma(0)->as_svg( { size => [ 500, 375 ] } )
		: '';
	$c->forward('View::SVG');
}

=head2 stemmadot

 GET /stemmadot/$textid
 
Returns the 'dot' format representation of the current stemma hypothesis.

=cut

sub stemmadot :Local :Args(1) {
	my( $self, $c, $textid ) = @_;
	my $m = $c->model('Directory');
	my $tradition = $m->tradition( $textid );
	
	$c->response->body( $tradition->stemma->editable );
	$c->forward('View::Plain');
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
