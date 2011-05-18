package Text::Tradition::Collation;

use Graph::Easy;
use IPC::Run qw( run binary );
use Text::Tradition::Collation::Reading;
use Moose;

has 'graph' => (
    is => 'ro',
    isa => 'Graph::Easy',
    handles => {
	add_reading => 'add_node',
	del_reading => 'del_node',
	add_path => 'add_edge',
	del_path => 'del_edge',
	reading => 'node',
	path => 'edge',
	readings => 'nodes',
	paths => 'edges',
    },
    default => sub { Graph::Easy->new( undirected => 0 ) },
    );
		

has 'tradition' => (
    is => 'rw',
    isa => 'Text::Tradition',
    );

has 'svg' => (
    is => 'ro',
    isa => 'Str',
    writer => '_save_svg',
    predicate => 'has_svg',
    );

has 'graphviz' => (
    is => 'ro',
    isa => 'Str',
    writer => '_save_graphviz',
    predicate => 'has_graphviz',
    );

has 'graphml' => (
    is => 'ro',
    isa => 'XML::LibXML::Document',
    writer => '_save_graphml',
    predicate => 'has_graphml',
    );

# Keeps track of the lemmas within the collation.  At most one lemma
# per position in the graph.
has 'lemmata' => (
    is => 'ro',
    isa => 'HashRef[Maybe[Str]]',
    default => sub { {} },
    );

has 'wit_list_separator' => (
			     is => 'rw',
			     isa => 'Str',
			     default => ', ',
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
    $self->graph->use_class('node', 'Text::Tradition::Collation::Reading');

    # Pass through any graph-specific options.
    my $shape = exists( $args->{'shape'} ) ? $args->{'shape'} : 'ellipse';
    $self->graph->set_attribute( 'node', 'shape', $shape );
}

# Wrappes around merge_nodes

sub merge_readings {
    my $self = shift;
    my $first_node = shift;
    my $second_node = shift;
    $first_node->merge_from( $second_node );
    unshift( @_, $first_node, $second_node );
    return $self->graph->merge_nodes( @_ );
}

=head2 Output method(s)

=over

=item B<as_svg>

print $graph->as_svg( $recalculate );

Returns an SVG string that represents the graph.  Uses GraphViz to do
this, because Graph::Easy doesn\'t cope well with long graphs. Unless
$recalculate is passed (and is a true value), the method will return a
cached copy of the SVG after the first call to the method.

=cut

sub as_svg {
    my( $self, $recalc ) = @_;
    return $self->svg if $self->has_svg;
    
    $self->_save_graphviz( $self->graph->as_graphviz() )
	unless( $self->has_graphviz && !$recalc );
    
    my @cmd = qw/dot -Tsvg/;
    my( $svg, $err );
    my $in = $self->graphviz;
    run( \@cmd, \$in, ">", binary(), \$svg );
    $self->{'svg'} = $svg;
    return $svg;
}

=item B<as_graphml>

print $graph->as_graphml( $recalculate )

Returns a GraphML representation of the collation graph, with
transposition information and position information. Unless
$recalculate is passed (and is a true value), the method will return a
cached copy of the SVG after the first call to the method.

=cut

sub as_graphml {
    my( $self, $recalc ) = @_;
    return $self->graphml if $self->has_graphml;

    # Some namespaces
    my $graphml_ns = 'http://graphml.graphdrawing.org/xmlns';
    my $xsi_ns = 'http://www.w3.org/2001/XMLSchema-instance';
    my $graphml_schema = 'http://graphml.graphdrawing.org/xmlns ' .
	'http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd';

    # Create the document and root node
    my $graphml = XML::LibXML->createDocument( "1.0", "UTF-8" );
    my $root = $graphml->createElementNS( $graphml_ns, 'graphml' );
    $graphml->setDocumentElement( $root );
    $root->setNamespace( $xsi_ns, 'xsi', 0 );
    $root->setAttributeNS( $xsi_ns, 'schemaLocation', $graphml_schema );

    # Add the data keys for nodes
    my @node_data = ( 'name', 'token', 'identical', 'position' );
    foreach my $ndi ( 0 .. $#node_data ) {
	my $key = $root->addNewChild( $graphml_ns, 'key' );
	$key->setAttribute( 'attr.name', $node_data[$ndi] );
	$key->setAttribute( 'attr.type', 'string' );
	$key->setAttribute( 'for', 'node' );
	$key->setAttribute( 'id', 'd'.$ndi );
    }

    # Add the data keys for edges
    my %wit_hash;
    my $wit_ctr = 0;
    foreach my $wit ( $self->getWitnessList ) {
	my $wit_key = 'w' . $wit_ctr++;
	$wit_hash{$wit} = $wit_key;
	my $key = $root->addNewChild( $graphml_ns, 'key' );
	$key->setAttribute( 'attr.name', $wit );
	$key->setAttribute( 'attr.type', 'string' );
	$key->setAttribute( 'for', 'edge' );
	$key->setAttribute( 'id', $wit_key );
    }

    # Add the graph, its nodes, and its edges
    my $graph = $root->addNewChild( $graphml_ns, 'graph' );
    $graph->setAttribute( 'edgedefault', 'directed' );
    $graph->setAttribute( 'id', 'g0' ); # TODO make this meaningful
    $graph->setAttribute( 'parse.edgeids', 'canonical' );
    $graph->setAttribute( 'parse.edges', $self->edges() );
    $graph->setAttribute( 'parse.nodeids', 'canonical' );
    $graph->setAttribute( 'parse.nodes', $self->nodes() );
    $graph->setAttribute( 'parse.order', 'nodesfirst' );

    my $node_ctr = 0;
    my %node_hash;
    foreach my $n ( $self->readings ) {
	my %this_node_data = ();
	foreach my $ndi ( 0 .. $#node_data ) {
	    my $value;
	    $this_node_data{'d'.$ndi} = $n->name if $node_data[$ndi] eq 'name';
	    $this_node_data{'d'.$ndi} = $n->label 
		if $node_data[$ndi] eq 'token';
	    $this_node_data{'d'.$ndi} = $n->primary->name if $n->has_primary;
	    $this_node_data{'d'.$ndi} = 
		$self->{'positions'}->node_position( $n )
		if $node_data[$ndi] eq 'position';
	}
	my $node_el = $graph->addNewChild( $graphml_ns, 'node' );
	my $node_xmlid = 'n' . $node_ctr++;
	$node_hash{ $n->name } = $node_xmlid;
	$node_el->setAttribute( 'id', $node_xmlid );
	    
	foreach my $dk ( keys %this_node_data ) {
	    my $d_el = $node_el->addNewChild( $graphml_ns, 'data' );
	    $d_el->setAttribute( 'key', $dk );
	    $d_el->appendTextChild( $this_node_data{$dk} );
	}
    }

    foreach my $e ( $self->edges() ) {
	my( $name, $from, $to ) = ( $e->name,
				    $node_hash{ $e->from()->name() },
				    $node_hash{ $e->to()->name() } );
	my $edge_el = $graph->addNewChild( $graphml_ns, 'edge' );
	$edge_el->setAttribute( 'source', $from );
	$edge_el->setAttribute( 'target', $to );
	$edge_el->setAttribute( 'id', $name );
	# TODO Got to add the witnesses
    }

    # Return the thing
    $self->_save_graphml( $graphml );
    return $graphml;
}

=back

=head2 Navigation methods

=over

=item B<start>

my $beginning = $collation->start();

Returns the beginning of the collation, a meta-reading with label '#START#'.

=cut

sub start {
    # Return the beginning reading of the graph.
    my $self = shift;
    my( $new_start ) = @_;
    if( $new_start ) {
	$self->del_reading( '#START#' );
	$self->graph->rename_node( $new_start, '#START#' );
    }
    return $self->reading('#START#');
}

=item B<reading_sequence>

my @readings = $graph->reading_sequence( $first, $last, $path[, $alt_path] );

Returns the ordered list of readings, starting with $first and ending
with $last, along the given witness path.  If no path is specified,
assume that the path is that of the base text (if any.)

=cut

sub reading_sequence {
    my( $self, $start, $end, $witness, $backup ) = @_;

    $witness = 'base text' unless $witness;
    my @readings = ( $start );
    my %seen;
    my $n = $start;
    while( $n ne $end ) {
	if( exists( $seen{$n->name()} ) ) {
	    warn "Detected loop at " . $n->name();
	    last;
	}
	$seen{$n->name()} = 1;
	
	my $next = $self->next_reading( $n, $witness, $backup );
	warn "Did not find any path for $witness from reading " . $n->name
	    unless $next;
	push( @readings, $next );
	$n = $next;
    }
    # Check that the last reading is our end reading.
    my $last = $readings[$#readings];
    warn "Last reading found from " . $start->label() .
	" for witness $witness is not the end!"
	unless $last eq $end;
    
    return @readings;
}

=item B<next_reading>

my $next_reading = $graph->next_reading( $reading, $witpath );

Returns the reading that follows the given reading along the given witness
path.  TODO These are badly named.

=cut

sub next_reading {
    # Return the successor via the corresponding path.
    my $self = shift;
    return $self->_find_linked_reading( 'next', @_ );
}

=item B<prior_reading>

my $prior_reading = $graph->prior_reading( $reading, $witpath );

Returns the reading that precedes the given reading along the given witness
path.  TODO These are badly named.

=cut

sub prior_reading {
    # Return the predecessor via the corresponding path.
    my $self = shift;
    return $self->_find_linked_reading( 'prior', @_ );
}

sub _find_linked_reading {
    my( $self, $direction, $node, $path, $alt_path ) = @_;
    my @linked_paths = $direction eq 'next' 
	? $node->outgoing() : $node->incoming();
    return undef unless scalar( @linked_paths );
    
    # We have to find the linked path that contains all of the
    # witnesses supplied in $path.
    my( @path_wits, @alt_path_wits );
    @path_wits = $self->witnesses_of_label( $path ) if $path;
    @alt_path_wits = $self->witnesses_of_label( $alt_path ) if $alt_path;
    my $base_le;
    my $alt_le;
    foreach my $le ( @linked_paths ) {
	if( $le->name eq 'base text' ) {
	    $base_le = $le;
	} else {
	    my @le_wits = $self->witnesses_of_label( $le->name );
	    if( _is_within( \@path_wits, \@le_wits ) ) {
		# This is the right path.
		return $direction eq 'next' ? $le->to() : $le->from();
	    } elsif( _is_within( \@alt_path_wits, \@le_wits ) ) {
		$alt_le = $le;
	    }
	}
    }
    # Got this far? Return the alternate path if it exists.
    return $direction eq 'next' ? $alt_le->to() : $alt_le->from()
	if $alt_le;

    # Got this far? Return the base path if it exists.
    return $direction eq 'next' ? $base_le->to() : $base_le->from()
	if $base_le;

    # Got this far? We have no appropriate path.
    warn "Could not find $direction node from " . $node->label 
	. " along path $path";
    return undef;
}

# Some set logic.
sub _is_within {
    my( $set1, $set2 ) = @_;
    my $ret = 1;
    foreach my $el ( @$set1 ) {
	$ret = 0 unless grep { /^\Q$el\E$/ } @$set2;
    }
    return $ret;
}


## INITIALIZATION METHODS - for use by parsers
# Walk the paths for each witness in the graph, and return the nodes
# that the graph has in common.  If $using_base is true, some 
# different logic is needed.

sub walk_witness_paths {
    my( $self, $end ) = @_;
    # For each witness, walk the path through the graph.
    # Then we need to find the common nodes.  
    # TODO This method is going to fall down if we have a very gappy 
    # text in the collation.
    my $paths = {};
    my @common_readings;
    foreach my $wit ( @{$self->tradition->witnesses} ) {
	my $curr_reading = $self->start;
	my @wit_path = $self->reading_sequence( $self->start, $end, 
						$wit->sigil );
	$wit->path( \@wit_path );

	# Detect the common readings.
	if( @common_readings ) {
	    my @cn;
	    foreach my $n ( @wit_path ) {
		push( @cn, $n ) if grep { $_ eq $n } @common_readings;
	    }
	    @common_readings = ();
	    push( @common_readings, @cn );
	} else {
	    push( @common_readings, @wit_path );
	}
    }

    # Mark all the nodes as either common or not.
    foreach my $cn ( @common_readings ) {
	print STDERR "Setting " . $cn->name . " / " . $cn->label 
	    . " as common node\n";
	$cn->make_common;
    }
    foreach my $n ( $self->readings() ) {
	$n->make_variant unless $n->is_common;
    }
    # Return an array of the common nodes in order.
    return @common_readings;
}

# An alternative to walk_witness_paths, for use when a collation is
# constructed from a base text and an apparatus.  Also modifies the
# collation graph to remove all 'base text' paths and replace them
# with real witness paths.

sub walk_and_expand_base {
    my( $self, $end ) = @_;

    foreach my $wit ( @{$self->tradition->witnesses} ) {
	my $sig = $wit_sigil;
	my $post_sig;
	$post_sig = $wit->post_correctione 
	    if $wit->has_post_correctione;
	my @wit_path = ( $self->start );
	my @wit_pc_path;
	my $curr_rdg = $self->start;
	my %seen;
	while( $curr_rdg ne $end ) {
	    if( $seen{$curr_reading->name} ) {
		warn "Detected loop in walk_and_expand_base with witness "
		    . "$sig on reading " . $curr_reading->name . "\n";
		last;
	    }
	    my $next_rdg = $self->next_reading( $curr_reading, $sig );
	    unless( $self->has_explicit_path( $curr_reading, 
					      $next_reading, $sig ) ) {
		$self->add_path( $curr_reading, $next_reading, $sig );
	    }
	    push( @wit_path, $next_reading );
	    $seen{$curr_reading->name} = 1;
	}
	$wit->path( \@wit_path );

	# Now go through this path and look for p.c. divergences.
	# TODO decide how to handle p.c. paths
	# BIG TODO handle case where p.c. follows the base and a.c. doesn't!
		
	    
}

sub common_readings {
    my $self = shift;
    my @common = grep { $_->is_common } $self->readings();
    return sort { _cmp_position( $a->position, $b->position ) } @common;
}

# Calculate the relative positions of nodes in the graph, if they
# were not given to us.
sub calculate_positions {
    my( $self, @ordered_common ) = @_;

    # We have to calculate the position identifiers for each word,
    # keyed on the common nodes.  This will be 'fun'.  The end result
    # is a hash per witness, whose key is the word node and whose
    # value is its position in the text.  Common nodes are always N,1
    # so have identical positions in each text.

    my $node_pos = {};
    foreach my $wit ( @{$self->tradition->witnesses} ) {
	# First we walk each path, making a matrix for each witness that
	# corresponds to its eventual position identifier.  Common nodes
	# always start a new row, and are thus always in the first column.

	my $wit_matrix = [];
	my $cn = 0;  # We should hit the common readings in order.
	my $row = [];
	foreach my $wn ( @{$wit->path} ) {
	    if( $wn eq $ordered_common[$cn] ) {
		# Set up to look for the next common node, and
		# start a new row of words.
		$cn++;
		push( @$wit_matrix, $row ) if scalar( @$row );
		$row = [];
	    }
	    push( @$row, $wn );
	}
	push( @$wit_matrix, $row );  # Push the last row onto the matrix

	# Now we have a matrix per witness, so that each row in the
	# matrix begins with a common node, and continues with all the
	# variant words that appear in the witness.  We turn this into
	# real positions in row,cell format.  But we need some
	# trickery in order to make sure that each node gets assigned
	# to only one position.

	foreach my $li ( 1..scalar(@$wit_matrix) ) {
	    foreach my $di ( 1..scalar(@{$wit_matrix->[$li-1]}) ) {
		my $reading = $wit_matrix->[$li-1]->[$di-1];
		my $position = "$li,$di";
		# If we have seen this node before, we need to compare
		# its position with what went before.
		unless( $reading->has_position &&
			_cmp_position( $position, $reading->position ) < 1 ) {
		    # The new position ID replaces the old one.
		    $reading->position( $position );
		} # otherwise, the old position needs to stay.
	    }
	}
    }

    $self->init_lemmata();
}

sub _cmp_position {
    my( $a, $b ) = @_;
    if ( $a && $b ) {
	my @pos_a = split(/,/, $a );
	my @pos_b = split(/,/, $b );

	my $big_cmp = $pos_a[0] <=> $pos_b[0];
	return $big_cmp if $big_cmp;
	# else 
	return $pos_a[1] <=> $pos_b[1];
    } elsif ( $b ) { # a is undefined
	return -1;
    } elsif ( $a ) { # b is undefined
	return 1;
    }
    return 0; # they are both undefined
}

sub all_positions {
    my $self = shift;
    my %positions = ();
    map { $positions{$_->position} = 1 } $self->readings;
    my @answer = sort { _cmp_position( $a, $b ) } keys( %positions );
    return @answer;
}

sub readings_at_position {
    my( $self, $pos ) = @_;
    my @answer = grep { $_->position eq $pos } $self->readings;
    return @answer;
}

## Lemmatizer functions

sub init_lemmata {
    my $self = shift;
    
    foreach my $position ( $self->all_positions ) {
	$self->lemmata->{$position} = undef;
    }

    foreach my $cr ( $self->common_readings ) {
	$self->lemmata->{$cr->position} = $cr->name;
    }
}
    
=item B<lemma_readings>

my @state = $graph->lemma_readings( @readings_delemmatized );

Takes a list of readings that have just been delemmatized, and returns
a set of tuples of the form ['reading', 'state'] that indicates what
changes need to be made to the graph.

=over

=item * 

A state of 1 means 'lemmatize this reading'

=item * 

A state of 0 means 'delemmatize this reading'

=item * 

A state of undef means 'an ellipsis belongs in the text here because
no decision has been made / an earlier decision was backed out'

=back

=cut

sub lemma_readings {
    my( $self, @toggled_off_nodes ) = @_;

    # First get the positions of those nodes which have been
    # toggled off.
    my $positions_off = {};
    map { $positions_off->{ $_->position } = $_->name } @toggled_off_nodes;

    # Now for each position, we have to see if a node is on, and we
    # have to see if a node has been turned off.
    my @answer;
    foreach my $pos ( $self->all_positions() ) {
	# Find the state of this position.  If there is an active node,
	# its name will be the state; otherwise the state will be 0 
	# (nothing at this position) or undef (ellipsis at this position)
	my $active = $self->lemmata->{$pos};
	
	# Is there a formerly active node that was toggled off?
	if( exists( $positions_off->{$pos} ) ) {
	    my $off_node = $positions_off->{$pos};
	    if( $active && $active ne $off_node) {
		push( @answer, [ $off_node, 0 ], [ $active, 1 ] );
	    } else {
		push( @answer, [ $off_node, $active ] );
	    }

	# No formerly active node, so we just see if there is a currently
	# active one.
	} elsif( $active ) {
	    # Push the active node, whatever it is.
	    push( @answer, [ $active, 1 ] );
	} else {
	    # Push the state that is there. Arbitrarily use the first node
	    # at that position.
	    my @pos_nodes = $self->readings_at_position( $pos );
	    push( @answer, [ $pos_nodes[0]->name, $self->lemmata->{$pos} ] );
	}
    }
    
    return @answer;
}

=item B<toggle_reading>

my @readings_delemmatized = $graph->toggle_reading( $reading_name );

Takes a reading node name, and either lemmatizes or de-lemmatizes
it. Returns a list of all readings that are de-lemmatized as a result
of the toggle.

=cut

sub toggle_reading {
    my( $self, $rname ) = @_;
    
    return unless $rname;
    my $reading = $self->reading( $rname );
    if( !$reading || $reading->is_common() ) {
	# Do nothing, it's a common node.
	return;
    } 
    
    my $pos = $reading->position;
    my $old_state = $self->lemmata->{$pos};
    my @readings_off;
    if( $old_state && $old_state eq $rname ) {
	# Turn off the node. We turn on no others by default.
	push( @readings_off, $reading );
    } else {
	# Turn on the node.
	$self->lemmata->{$pos} = $rname;
	# Any other 'on' readings in the same position should be off.
	push( @readings_off, $self->same_position_as( $reading ) );
	# Any node that is an identical transposed one should be off.
	push( @readings_off, $reading->identical_readings );
    }
    @readings_off = unique_list( @readings_off );

    # Turn off the readings that need to be turned off.
    my @readings_delemmatized;
    foreach my $n ( @readings_off ) {
	my $state = $self->lemmata->{$n->position};
	if( $state && $state eq $n->name ) { 
	    # this reading is still on, so turn it off
	    push( @readings_delemmatized, $n );
	    my $new_state = undef;
	    if( $n eq $reading ) {
		# This is the reading that was clicked, so if there are no
		# other readings there, turn off the position.  In all other
		# cases, restore the ellipsis.
		my @other_n = $self->same_position_as( $n );
		$new_state = 0 unless @other_n;
	    }
	    $self->lemmata->{$n->position} = $new_state;
	} elsif( $old_state && $old_state eq $n->name ) { 
	    # another reading has already been turned on here
	    push( @readings_delemmatized, $n );
	} # else some other reading was on anyway, so pass.
    }
    return @readings_delemmatized;
}

sub same_position_as {
    my( $self, $reading ) = @_;
    my $pos = $reading->position;
    my @same = grep { $_ ne $reading } $self->readings_at_position( $reading->position );
    return @same;
}

# Return the string that joins together a list of witnesses for
# display on a single path.
sub path_label {
    my $self = shift;
    return join( $self->wit_list_separator, @_ );
}

sub witnesses_of_label {
    my( $self, $label ) = @_;
    my $regex = $self->wit_list_separator;
    my @answer = split( /\Q$regex\E/, $label );
    return @answer;
}    

sub unique_list {
    my( @list ) = @_;
    my %h;
    map { $h{$_->name} = $_ } @list;
    return values( %h );
}

no Moose;
__PACKAGE__->meta->make_immutable;
