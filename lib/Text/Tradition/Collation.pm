package Text::Tradition::Collation;

use Graph::Easy;
use Moose;

has 'graph' => (
    is => 'ro',
    isa => 'Graph::Easy',
    handles => {
	add_node => 'add_reading',
	del_node => 'del_reading',
	add_edge => 'add_path',
	del_edge => 'del_path',
	nodes => 'readings',
	edges => 'paths',
    },
    default => sub { Graph::Easy->new( undirected => 0 ) },
    );
		

has 'tradition' => (
    is => 'ro',
    isa => 'Text::Tradition',
    );

# The collation can be created two ways:
# 1. Collate a set of witnesses (with CollateX I guess) and process
#    the results as in 2.
# 2. Read a pre-prepared collation in one of a variety of formats,
#    and make the graph from that.

# The graph itself will (for now) be immutable, and the positions
# within the graph will also be immutable.  We need to calculate those
# positions upon graph construction.  The equivalences between graph
# nodes will be mutable, entirely determined by the user (or possibly
# by some semantic pre-processing provided by the user.)  So the
# constructor should just make an empty equivalences object.  The
# constructor will also need to make the witness objects, if we didn't
# come through option 1.

sub BUILD {
    my( $self, $args ) = @_;

    # Call the appropriate parser on the given data
    my @formats = grep { /^(GraphML|CSV|CTE|TEI)$/ } keys( %$args );
    my $format = shift( @formats );
    unless( $format ) {
	warn "No data given to create a graph; will initialize an empty one";
    }
    if( $format =~ /^(CSV|CTE)$/ && !exists $args->{'base'} ) {
	warn "Cannot make a graph from $format without a base text";
	return;
    }

    # Initialize our graph object.
    $self->graph->set_attribute( 'node', 'shape', 'ellipse' );
    # Starting point for all texts
    my $last_node = $self->graph->add_node( '#START#' );

    # Now do the parsing.
    my @sigla;
    if( $format ) {
	my @parseargs;
	if( $format =~ /^(CSV|CTE)$/ ) {
	    @parseargs = ( 'base' => $args->{'base'},
		      'data' => $args->{$format},
		      'format' => $format );
	    $format = 'BaseText';
	} else {
	    @parseargs = ( $args->{ $format } ); 
	}
	my $mod = "Text::Tradition::Parser::$format";
	load( $mod );
	# TODO parse needs to return witness IDs
	@sigla = $mod->can('parse')->( $self->graph, @parseargs );
    }

    # Do we need to initialize the witnesses?
    unless( $args->{'have_witnesses'} ) {
	# initialize Witness objects for all our witnesses
	my @witnesses;
	foreach my $sigil ( @sigla ) {
	    push( @witnesses, Text::Tradition::Witness->new( 'sigil' => $sigil ) );
	}
	$self->tradition->witnesses( \@witnesses );
    }
}

# Wrappers around some methods

sub merge_readings {
    my $self = shift;
    my $first_node = shift;
    my $second_node = shift;
    $first_node->merge_from( $second_node );
    unshift( @_, $first_node, $second_node );
    return $self->graph->merge_nodes( @_ );
}

no Moose;
__PACKAGE__->meta->make_immutable;
