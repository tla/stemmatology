package Text::Tradition::Stemma;

use Bio::Phylo::IO;
use Encode qw( decode_utf8 );
use File::Temp;
use Graph;
use Graph::Reader::Dot;
use IPC::Run qw/ run binary /;
use Text::Tradition::Error;
use Text::Tradition::StemmaUtil qw/ character_input phylip_pars parse_newick /;
use Moose;

has collation => (
    is => 'ro',
    isa => 'Text::Tradition::Collation',
    required => 1,
    weak_ref => 1,
    );  

has graph => (
    is => 'rw',
    isa => 'Graph',
    predicate => 'has_graph',
    );
    
has distance_trees => (
    is => 'ro',
    isa => 'ArrayRef[Graph]',
    writer => '_save_distance_trees',
    predicate => 'has_distance_trees',
    );
    
has distance_program => (
	is => 'rw',
	isa => 'Str',
	default => '',
	);
    
sub BUILD {
    my( $self, $args ) = @_;
    # If we have been handed a dotfile, initialize it into a graph.
    if( exists $args->{'dot'} ) {
        $self->graph_from_dot( $args->{'dot'} );
    }
}

sub graph_from_dot {
	my( $self, $dotfh ) = @_;
 	my $reader = Graph::Reader::Dot->new();
	my $graph = $reader->read_graph( $dotfh );
	if( $graph ) {
		$self->graph( $graph );
		# Go through the nodes and set any non-hypothetical node to extant.
		foreach my $v ( $self->graph->vertices ) {
			$self->graph->set_vertex_attribute( $v, 'class', 'extant' )
				unless $self->graph->has_vertex_attribute( $v, 'class' );
		}
	} else {
		throw( "Failed to parse dot in $dotfh" );
	}
}

sub as_dot {
    my( $self, $opts ) = @_;
    
    # Get default and specified options
    my %graphopts = ();
    my %nodeopts = (
		'fontsize' => 11,
		'hshape' => 'plaintext',	# Shape for the hypothetical nodes
		'htext' => '*',
		'style' => 'filled',
		'fillcolor' => 'white',
		'shape' => 'ellipse',	# Shape for the extant nodes
	);
	my %edgeopts = (
		'arrowhead' => 'open',
	);
	@graphopts{ keys %{$opts->{'graph'}} } = values %{$opts->{'graph'}} 
		if $opts->{'graph'};
	@nodeopts{ keys %{$opts->{'node'}} } = values %{$opts->{'node'}} 
		if $opts->{'node'};
	@edgeopts{ keys %{$opts->{'edge'}} } = values %{$opts->{'edge'}} 
		if $opts->{'edge'};

	my @dotlines;
	push( @dotlines, 'digraph stemma {' );
	## Print out the global attributes
	push( @dotlines, _make_dotline( 'graph', %graphopts ) ) if keys %graphopts;
	push( @dotlines, _make_dotline( 'edge', %edgeopts ) ) if keys %edgeopts;
	## Delete our special attributes from the node set before continuing
	my $hshape = delete $nodeopts{'hshape'};
	my $htext = delete $nodeopts{'htext'};
	push( @dotlines, _make_dotline( 'node', %nodeopts ) ) if keys %nodeopts;

	# Add each of the nodes.
    foreach my $n ( $self->graph->vertices ) {
        if( $self->graph->get_vertex_attribute( $n, 'class' ) eq 'hypothetical' ) {
        	# Apply our display settings for hypothetical nodes.
        	push( @dotlines, _make_dotline( $n, 'shape' => $hshape, 'label' => $htext ) );
        } else {
        	# Use the default display settings.
            push( @dotlines, "  $n;" );
        }
    }
    # Add each of our edges.
    foreach my $e ( $self->graph->edges ) {
    	my( $from, $to ) = @$e;
    	push( @dotlines, "  $from -> $to;" );
    }
    push( @dotlines, '}' );
    
    return join( "\n", @dotlines );
}


# Another version of dot output meant for graph editing, thus
# much simpler.
sub editable {
	my $self = shift;
	my @dotlines;
	push( @dotlines, 'digraph stemma {' );
	my @real; # A cheap sort
    foreach my $n ( sort $self->graph->vertices ) {
    	my $c = $self->graph->get_vertex_attribute( $n, 'class' );
    	$c = 'extant' unless $c;
    	if( $c eq 'extant' ) {
    		push( @real, $n );
    	} else {
			push( @dotlines, _make_dotline( $n, 'class' => $c ) );
		}
    }
	# Now do the real ones
	foreach my $n ( @real ) {
		push( @dotlines, _make_dotline( $n, 'class' => 'extant' ) );
	}
	foreach my $e ( sort _by_vertex $self->graph->edges ) {
		my( $from, $to ) = @$e;
		push( @dotlines, "  $from -> $to;" );
	}
    push( @dotlines, '}' );
    return join( "\n", @dotlines );
}

sub _make_dotline {
	my( $obj, %attr ) = @_;
	my @pairs;
	foreach my $k ( keys %attr ) {
		my $v = $attr{$k};
		$v =~ s/\"/\\\"/g;
		push( @pairs, "$k=\"$v\"" );
	}
	return sprintf( "  %s [ %s ];", $obj, join( ', ', @pairs ) );
}
	
sub _by_vertex {
	return $a->[0].$a->[1] cmp $b->[0].$b->[1];
}

# Render the stemma as SVG.
sub as_svg {
    my( $self, $opts ) = @_;
    my $dot = $self->as_dot( $opts );
    my @cmd = qw/dot -Tsvg/;
    my( $svg, $err );
    my $dotfile = File::Temp->new();
    ## TODO REMOVE
    # $dotfile->unlink_on_destroy(0);
    binmode $dotfile, ':utf8';
    print $dotfile $dot;
    push( @cmd, $dotfile->filename );
    run( \@cmd, ">", binary(), \$svg );
    $svg = decode_utf8( $svg );
    return $svg;
}

sub witnesses {
    my $self = shift;
    my @wits = grep { $self->graph->get_vertex_attribute( $_, 'class' ) eq 'extant' }
        $self->graph->vertices;
    return @wits;
}

#### Methods for calculating phylogenetic trees ####

before 'distance_trees' => sub {
    my $self = shift;
    my %args = (
    	'program' => 'phylip_pars',
    	@_ );
    # TODO allow specification of method for calculating distance tree
    if( !$self->has_distance_trees
    	|| $args{'program'} ne $self->distance_program ) {
        # We need to make a tree before we can return it.
        my $dsub = 'run_' . $args{'program'};
        my( $ok, $result ) = $self->$dsub();
        if( $ok ) {
            # Save the resulting trees
            my $trees = parse_newick( $result );
            $self->_save_distance_trees( $trees );
            $self->distance_program( $args{'program'} );
        } else {
            throw( "Failed to calculate distance trees: $result" );
        }
    }
};

sub run_phylip_pars {
	my $self = shift;
	my $cdata = character_input( $self->collation->make_alignment_table() );
	return phylip_pars( $cdata );
}

sub throw {
	Text::Tradition::Error->throw( 
		'ident' => 'Stemma error',
		'message' => $_[0],
		);
}


no Moose;
__PACKAGE__->meta->make_immutable;
    
1;
