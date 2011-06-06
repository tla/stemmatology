package Text::Tradition::Collation;

use Graph::Easy;
use IPC::Run qw( run binary );
use Text::Tradition::Collation::Path;
use Text::Tradition::Collation::Position;
use Text::Tradition::Collation::Reading;
use Text::Tradition::Collation::Relationship;
use Text::Tradition::Collation::Segment;
use XML::LibXML;
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
	segments => 'nodes',
	paths => 'edges',
	relationships => 'edges',
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

has 'graphml' => (
    is => 'ro',
    isa => 'Str',
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

has 'baselabel' => (
    is => 'rw',
    isa => 'Str',
    default => 'base text',
    );

has 'collapsed' => (
    is => 'rw',
    isa => 'Bool',
    );

has 'linear' => (
    is => 'rw',
    isa => 'Bool',
    default => 1,
    );

has 'ac_label' => (
    is => 'rw',
    isa => 'Str',
    default => ' (a.c.)',
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
    $self->graph->use_class('edge', 'Text::Tradition::Collation::Path');

    # Pass through any graph-specific options.
    my $shape = exists( $args->{'shape'} ) ? $args->{'shape'} : 'ellipse';
    $self->graph->set_attribute( 'node', 'shape', $shape );
}

# Wrapper around add_path 

around add_path => sub {
    my $orig = shift;
    my $self = shift;

    # Make sure there are three arguments
    unless( @_ == 3 ) {
	warn "Call add_path with args source, target, witness";
	return;
    }
    # Make sure the proposed path does not yet exist
    # NOTE 'reading' will currently return readings and segments
    my( $source, $target, $wit ) = @_;
    $source = $self->reading( $source )
	unless ref( $source ) eq 'Text::Tradition::Collation::Reading';
    $target = $self->reading( $target )
	unless ref( $target ) eq 'Text::Tradition::Collation::Reading';
    foreach my $path ( $source->edges_to( $target ) ) {
	if( $path->label eq $wit && $path->class eq 'edge.path' ) {
	    return;
	}
    }
    # Do the deed
    $self->$orig( @_ );
};

# Wrapper around paths
around paths => sub {
    my $orig = shift;
    my $self = shift;

    my @result = grep { $_->sub_class eq 'path' } $self->$orig( @_ );
    return @result;
};

around relationships => sub {
    my $orig = shift;
    my $self = shift;
    my @result = grep { $_->sub_class eq 'relationship' } $self->$orig( @_ );
    return @result;
};

around readings => sub {
    my $orig = shift;
    my $self = shift;
    my @result = grep { $_->sub_class ne 'segment' } $self->$orig( @_ );
    return @result;
};

around segments => sub {
    my $orig = shift;
    my $self = shift;
    my @result = grep { $_->sub_class eq 'segment' } $self->$orig( @_ );
    return @result;
};

# Wrapper around merge_nodes

sub merge_readings {
    my $self = shift;
    my $first_node = shift;
    my $second_node = shift;
    $first_node->merge_from( $second_node );
    unshift( @_, $first_node, $second_node );
    return $self->graph->merge_nodes( @_ );
}

# Extra graph-alike utility
sub has_path {
    my( $self, $source, $target, $label ) = @_;
    my @paths = $source->edges_to( $target );
    my @relevant = grep { $_->label eq $label } @paths;
    return scalar @relevant;
}

## Dealing with groups of readings, i.e. segments.

sub add_segment {
    my( $self, @items ) = @_;
    my $segment = Text::Tradition::Collation::Segment->new( 'members' => \@items );
    return $segment;
}

## Dealing with relationships between readings.  This is a different
## sort of graph edge.  Return a success/failure value and a list of
## node pairs that have been linked.

sub add_relationship {
    my( $self, $source, $target, $options ) = @_;

    # Make sure there is not another relationship between these two
    # readings or segments already
    $source = $self->reading( $source )
	unless ref( $source ) && $source->isa( 'Graph::Easy::Node' );
    $target = $self->reading( $target )
	unless ref( $target ) && $target->isa( 'Graph::Easy::Node' );
    foreach my $rel ( $source->edges_to( $target ), $target->edges_to( $source ) ) {
	if( $rel->class eq 'edge.relationship' ) {
	    return ( undef, "Relationship already exists between these readings" );
	} else {
	    return ( undef, "There is a witness path between these readings" );
	}
    }

    if( $source->has_position && $target->has_position ) {
	unless( grep { $_ eq $target } $self->same_position_as( $source ) ) {
	    return( undef, "Cannot set relationship at different positions" );
	}
    }

    my @joined = ( [ $source->name, $target->name ] );  # Keep track of the nodes we join.
    
    $options->{'this_relation'} = [ $source, $target ];
    my $rel = Text::Tradition::Collation::Relationship->new( %$options );
    $self->graph->add_edge( $source, $target, $rel );
    if( $options->{'global'} ) {
	# Look for all readings with the source label, and if there are
	# colocated readings with the target label, join them too.
	foreach my $r ( grep { $_->label eq $source->label } $self->readings() ) {
	    next if $r->name eq $source->name;
	    my @colocated = grep { $_->label eq $target->label }
	        $self->same_position_as( $r );
	    if( @colocated ) {
		warn "Multiple readings with same label at same position!"
		    if @colocated > 1;
		my $colo = $colocated[0];
		next if $colo->edges_to( $r ) || $r->edges_to( $colo );
		$options->{'primary_relation'} = $options->{'this_relation'};
		$options->{'this_relation'} = [ $r, $colocated[0] ];
		my $dup_rel = Text::Tradition::Collation::Relationship->new( %$options );
		$self->graph->add_edge( $r, $colocated[0], $dup_rel );
		push( @joined, [ $r->name, $colocated[0]->name ] );
	    }
	}
    }
    return( 1, @joined );
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
    
    $self->collapse_graph_paths();
    
    my @cmd = qw/dot -Tsvg/;
    my( $svg, $err );
    my $in = $self->as_dot();
    run( \@cmd, \$in, ">", binary(), \$svg );
    $self->_save_svg( $svg );
    $self->expand_graph_paths();
    return $svg;
}

=item B<as_dot>

print $graph->as_dot( $view, $recalculate );

Returns a string that is the collation graph expressed in dot
(i.e. GraphViz) format.  The 'view' argument determines what kind of
graph is produced.
    * 'path': a graph of witness paths through the collation (DEFAULT)
    * 'relationship': a graph of how collation readings relate to 
      each other

=cut

sub as_dot {
    my( $self, $view ) = @_;
    $view = 'path' unless $view;
    # TODO consider making some of these things configurable
    my $dot = sprintf( "digraph %s {\n", $self->tradition->name );
    $dot .= "\tedge [ arrowhead=open ];\n";
    $dot .= "\tgraph [ rankdir=LR ];\n";
    $dot .= sprintf( "\tnode [ fontsize=%d, fillcolor=%s, style=%s, shape=%s ];\n",
		     11, "white", "filled", $self->graph->get_attribute( 'node', 'shape' ) );

    foreach my $reading ( $self->readings ) {
	# Need not output nodes without separate labels
	next if $reading->name eq $reading->label;
	# TODO output readings or segments, but not both
	next if $reading->class eq 'node.segment';
	$dot .= sprintf( "\t\"%s\" [ label=\"%s\" ]\n", $reading->name, $reading->label );
    }

    my @edges = $view eq 'relationship' ? $self->relationships : $self->paths;
    foreach my $edge ( @edges ) {
	$dot .= sprintf( "\t\"%s\" -> \"%s\" [ color=\"%s\", fontcolor=\"%s\", label=\"%s\" ]\n",
			 $edge->from->name, $edge->to->name, '#000000', '#000000', $edge->label );
    }

    $dot .= "}\n";
    return $dot;
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
    my %node_data_keys;
    my $ndi = 0;
    foreach my $datum ( qw/ name reading identical position class / ) {
	$node_data_keys{$datum} = 'dn'.$ndi++;
	my $key = $root->addNewChild( $graphml_ns, 'key' );
	$key->setAttribute( 'attr.name', $datum );
	$key->setAttribute( 'attr.type', 'string' );
	$key->setAttribute( 'for', 'node' );
	$key->setAttribute( 'id', $node_data_keys{$datum} );
    }

    # Add the data keys for edges, i.e. witnesses
    my $edi = 0;
    my %edge_data_keys;
    foreach my $edge_key( qw/ witness_main witness_ante_corr relationship class / ) {
	$edge_data_keys{$edge_key} = 'de'.$edi++;
	my $key = $root->addNewChild( $graphml_ns, 'key' );
	$key->setAttribute( 'attr.name', $edge_key );
	$key->setAttribute( 'attr.type', 'string' );
	$key->setAttribute( 'for', 'edge' );
	$key->setAttribute( 'id', $edge_data_keys{$edge_key} );
    }
    
    # Add the graph, its nodes, and its edges
    my $graph = $root->addNewChild( $graphml_ns, 'graph' );
    $graph->setAttribute( 'edgedefault', 'directed' );
    $graph->setAttribute( 'id', 'g0' ); # TODO make this meaningful
    $graph->setAttribute( 'parse.edgeids', 'canonical' );
    $graph->setAttribute( 'parse.edges', scalar($self->paths) );
    $graph->setAttribute( 'parse.nodeids', 'canonical' );
    $graph->setAttribute( 'parse.nodes', scalar($self->readings) );
    $graph->setAttribute( 'parse.order', 'nodesfirst' );

    my $node_ctr = 0;
    my %node_hash;
    # Add our readings to the graph
    foreach my $n ( sort { $a->name cmp $b->name } $self->readings ) {
	my $node_el = $graph->addNewChild( $graphml_ns, 'node' );
	my $node_xmlid = 'n' . $node_ctr++;
	$node_hash{ $n->name } = $node_xmlid;
	$node_el->setAttribute( 'id', $node_xmlid );
	_add_graphml_data( $node_el, $node_data_keys{'name'}, $n->name );
	_add_graphml_data( $node_el, $node_data_keys{'reading'}, $n->label );
	_add_graphml_data( $node_el, $node_data_keys{'position'}, $n->position );
	_add_graphml_data( $node_el, $node_data_keys{'class'}, $n->sub_class );
	_add_graphml_data( $node_el, $node_data_keys{'identical'}, $n->primary->name )
	    if $n->has_primary;
    }

    # Add any segments we have
    foreach my $n ( sort { $a->name cmp $b->name } $self->segments ) {
	my $node_el = $graph->addNewChild( $graphml_ns, 'node' );
	my $node_xmlid = 'n' . $node_ctr++;
	$node_hash{ $n->name } = $node_xmlid;
	$node_el->setAttribute( 'id', $node_xmlid );
	_add_graphml_data( $node_el, $node_data_keys{'class'}, $n->sub_class );
	_add_graphml_data( $node_el, $node_data_keys{'name'}, $n->name );
    }

    # Add the path, relationship, and segment edges
    my $edge_ctr = 0;
    foreach my $e ( sort { $a->from->name cmp $b->from->name } $self->graph->edges() ) {
	my( $name, $from, $to ) = ( 'e'.$edge_ctr++,
				    $node_hash{ $e->from->name() },
				    $node_hash{ $e->to->name() } );
	my $edge_el = $graph->addNewChild( $graphml_ns, 'edge' );
	$edge_el->setAttribute( 'source', $from );
	$edge_el->setAttribute( 'target', $to );
	$edge_el->setAttribute( 'id', $name );
	# Add the edge class
	_add_graphml_data( $edge_el, $edge_data_keys{'class'}, $e->sub_class );
	if( $e->sub_class eq 'path' ) {
	    # It's a witness path, so add the witness
	    my $base = $e->label;
	    my $key = $edge_data_keys{'witness_main'};
	    # TODO kind of hacky
	    if( $e->label =~ /^(.*?)\s+(\(a\.c\.\))$/ ) {
		$base = $1;
		$key = $edge_data_keys{'witness_ante_corr'};
	    }
	    _add_graphml_data( $edge_el, $key, $base );
	} elsif( $e->sub_class eq 'relationship' ) {
	    # It's a relationship
	    _add_graphml_data( $edge_el, $edge_data_keys{'relationship'}, $e->label );
	} # else a segment, nothing to record but source, target, class
    }

    # Return the thing
    $self->_save_graphml( $graphml->toString(1) );
    return $graphml->toString(1);
}

sub _add_graphml_data {
    my( $el, $key, $value ) = @_;
    my $data_el = $el->addNewChild( $el->namespaceURI, 'data' );
    return unless defined $value;
    $data_el->setAttribute( 'key', $key );
    $data_el->appendText( $value );
}

sub collapse_graph_paths {
    my $self = shift;
    # Our collation graph has an path per witness.  This is great for
    # calculation purposes, but terrible for display.  Thus we want to
    # display only one path between any two nodes.

    return if $self->collapsed;

    print STDERR "Collapsing witness paths in graph...\n";

    # Don't list out every witness if we have more than half to list.
    my $majority = int( scalar( @{$self->tradition->witnesses} ) / 2 ) + 1;
    # But don't compress if there are only a few witnesses.
    $majority = 4 if $majority < 4;
    foreach my $node ( $self->readings ) {
	my $newlabels = {};
	# We will visit each node, so we only look ahead.
	foreach my $edge ( $node->outgoing() ) {
	    next unless $edge->class eq 'edge.path';
	    add_hash_entry( $newlabels, $edge->to->name, $edge->name );
	    $self->del_path( $edge );
	}

	foreach my $newdest ( keys %$newlabels ) {
	    my $label;
	    my @compressed_wits = ();
	    if( @{$newlabels->{$newdest}} < $majority ) {
		$label = join( ', ', sort( @{$newlabels->{$newdest}} ) );
	    } else {
		## TODO FIX THIS HACK
		my @aclabels;
		foreach my $wit ( @{$newlabels->{$newdest}} ) {
		    if( $wit =~ /^(.*?)(\s*\(?a\.\s*c\.\)?)$/ ) {
			push( @aclabels, $wit );
		    } else {
			push( @compressed_wits, $wit );
		    }
		}
		$label = join( ', ', 'majority', sort( @aclabels ) );
	    }
	    
	    my $newpath = 
		$self->add_path( $node, $self->reading( $newdest ), $label );
	    if( @compressed_wits ) {
		$newpath->hidden_witnesses( \@compressed_wits );
	    }
	}
    }

    $self->collapsed( 1 );
}

sub expand_graph_paths {
    my $self = shift;
    # Our collation graph has only one path between any two nodes.
    # This is great for display, but not so great for analysis.
    # Expand this so that each witness has its own path between any
    # two reading nodes.
    return unless $self->collapsed;
    
    print STDERR "Expanding witness paths in graph...\n";
    foreach my $path( $self->paths ) {
	my $from = $path->from;
	my $to = $path->to;
	my @wits = split( /, /, $path->label );
	if( $path->has_hidden_witnesses ) {
	    push( @wits, @{$path->hidden_witnesses} );
	}
	$self->del_path( $path );
	foreach ( @wits ) {
	    $self->add_path( $from, $to, $_ );
	}
    }
    $self->collapsed( 0 );
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

    $witness = $self->baselabel unless $witness;
    my @readings = ( $start );
    my %seen;
    my $n = $start;
    while( $n && $n ne $end ) {
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
path.  

=cut

sub next_reading {
    # Return the successor via the corresponding path.
    my $self = shift;
    return $self->_find_linked_reading( 'next', @_ );
}

=item B<prior_reading>

my $prior_reading = $graph->prior_reading( $reading, $witpath );

Returns the reading that precedes the given reading along the given witness
path.  

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
	if( $le->name eq $self->baselabel ) {
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
    my $ret = @$set1; # will be 0, i.e. false, if set1 is empty
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
	@common_readings = _find_common( \@common_readings, \@wit_path );
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

sub _find_common {
    my( $common_readings, $new_path ) = @_;
    my @cr;
    if( @$common_readings ) {
	foreach my $n ( @$new_path ) {
	    push( @cr, $n ) if grep { $_ eq $n } @$common_readings;
	}
    } else {
	push( @cr, @$new_path );
    }
    return @cr;
}

sub _remove_common {
    my( $common_readings, $divergence ) = @_;
    my @cr;
    my %diverged;
    map { $diverged{$_->name} = 1 } @$divergence;
    foreach( @$common_readings ) {
	push( @cr, $_ ) unless $diverged{$_->name};
    }
    return @cr;
}


# An alternative to walk_witness_paths, for use when a collation is
# constructed from a base text and an apparatus.  We have the
# sequences of readings and just need to add path edges.

sub make_witness_paths {
    my( $self ) = @_;

    my @common_readings;
    foreach my $wit ( @{$self->tradition->witnesses} ) {
	print STDERR "Making path for " . $wit->sigil . "\n";
	$self->make_witness_path( $wit );
	@common_readings = _find_common( \@common_readings, $wit->path );
	@common_readings = _find_common( \@common_readings, $wit->uncorrected_path );
    }
    map { $_->make_common } @common_readings;
    return @common_readings;
}

sub make_witness_path {
    my( $self, $wit ) = @_;
    my @chain = @{$wit->path};
    my $sig = $wit->sigil;
    foreach my $idx ( 0 .. $#chain-1 ) {
	$self->add_path( $chain[$idx], $chain[$idx+1], $sig );
    }
    @chain = @{$wit->uncorrected_path};
    foreach my $idx( 0 .. $#chain-1 ) {
	my $source = $chain[$idx];
	my $target = $chain[$idx+1];
	$self->add_path( $source, $target, $sig.$self->ac_label )
	    unless $self->has_path( $source, $target, $sig );
    }
}

sub common_readings {
    my $self = shift;
    my @common = grep { $_->is_common } $self->readings();
    return sort { $a->position->cmp_with( $b->position ) } @common;
}

# Calculate the relative positions of nodes in the graph, if they
# were not given to us.
sub calculate_positions {
    my( $self, @ordered_common ) = @_;

    # First assign positions to all the common nodes.
    my $l = 1;
    foreach my $oc ( @ordered_common ) {
	$oc->position( $l++, 1 );
    }

    if( $self->linear ) {
	# For the space between each common node, we have to find all the chains
	# from all the witnesses.  The longest chain gives us our max, and the
	# others get min/max ranges to fit.
	my $first = shift @ordered_common;
	while( @ordered_common ) {
	    my %paths;
	    my $next = shift @ordered_common;
	    my $longest = 0;
	    foreach my $wit ( @{$self->tradition->witnesses} ) {
		# Key to the path is not important; we just have to get
		# all unique paths.
		my $length = $self->_track_paths( \%paths, $first, $next, $wit->sigil );
		$longest = $length unless $longest > $length;
		if( $wit->has_ante_corr ) {
		    my $length = $self->_track_paths( \%paths, $first, $next, 
						      $wit->sigil.$self->ac_label, $wit->sigil );
		    $longest = $length unless $longest > $length;
		}
	    }
	    
	    # Transform the path values from unique strings to arrays.
	    foreach my $k ( keys %paths ) {
		my @v = split( /\s+/, $paths{$k} );
		$paths{$k} = \@v;
	    }
	    
	    # Now %paths has all the unique paths, and we know how long the
	    # longest of these is.  Assign positions, starting with the
	    # longest.  All non-common positions start at 2.
	    foreach my $path ( sort { scalar @$b <=> scalar @$a } values %paths  ) {
		my $range = $longest - scalar @$path;
		foreach my $i ( 0 .. $#{$path} ) {
		    my $min = $i+2;
		    my $rdg = $self->reading( $path->[$i] );
		    unless( $rdg->has_position ) {
			$rdg->position( $first->position->common, $min, $min+$range );
		    }
		}
	    }
	    
	    $first = $next;
	}
    } else {

	# Non-linear positions are pretty much impossible to pin down.
	# Any reading might appear anywhere in the graph.  I guess we
	# can do positions where there aren't transpositions...

    }
		
    $self->init_lemmata();
}

# Helper function for the guts of calculate_positions.
sub _track_paths {
    my $self = shift;
    my $track_hash = shift;
    # Args are first, last, wit, backup
    my @path = $self->reading_sequence( @_ );
    # Top and tail the array
    shift @path;
    pop @path;
    $track_hash->{$_[2]} = join( ' ', map { $_->name } @path )
	if @path;
    return @path;
}
 
sub possible_positions {
    my $self = shift;
    my @answer;
    my %positions = ();
    foreach my $r ( $self->readings ) {
	next unless $r->has_position;
	$positions{$r->position->maxref} = 1;
    }
    @answer = keys %positions;
    return @answer;
}

# TODO think about indexing this.
sub readings_at_position {
    my( $self, $position, $strict ) = @_;
    unless( ref( $position ) eq 'Text::Tradition::Collation::Position' ) {
	$position = Text::Tradition::Collation::Position->new( $position );
    }
    my @answer;
    foreach my $r ( $self->readings ) {
	push( @answer, $r ) if $r->is_at_position( $position, $strict );
    }
    return @answer;
}

## Lemmatizer functions

sub init_lemmata {
    my $self = shift;

    foreach my $position ( $self->possible_positions ) {
	$self->lemmata->{$position} = undef;
    }

    foreach my $cr ( $self->common_readings ) {
	$self->lemmata->{$cr->position->maxref} = $cr->name;
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
    map { $positions_off->{ $_->position->reference } = $_->name } 
        @toggled_off_nodes;

    # Now for each position, we have to see if a node is on, and we
    # have to see if a node has been turned off.  The lemmata hash
    # should contain fixed positions, range positions whose node was
    # just turned off, and range positions whose node is on.
    my @answer;
    my %fixed_positions;
    # TODO One of these is probably redundant.
    map { $fixed_positions{$_} = 0 } keys %{$self->lemmata};
    map { $fixed_positions{$_} = 0 } keys %{$positions_off};
    map { $fixed_positions{$_} = 1 } $self->possible_positions;
    foreach my $pos ( sort { Text::Tradition::Collation::Position::str_cmp( $a, $b ) } keys %fixed_positions ) {
	# Find the state of this position.  If there is an active node,
	# its name will be the state; otherwise the state will be 0 
	# (nothing at this position) or undef (ellipsis at this position)
	my $active = undef;
	$active = $self->lemmata->{$pos} if exists $self->lemmata->{$pos};
	
	# Is there a formerly active node that was toggled off?
	if( exists( $positions_off->{$pos} ) ) {
	    my $off_node = $positions_off->{$pos};
	    if( $active && $active ne $off_node) {
		push( @answer, [ $off_node, 0 ], [ $active, 1 ] );
	    } else {
		unless( $fixed_positions{$pos} ) {
		    $active = 0;
		    delete $self->lemmata->{$pos};
		}
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
	    delete $self->lemmata->{$pos} unless $fixed_positions{$pos};
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
    my $fixed = $reading->position->fixed;
    my $old_state = $self->lemmata->{$pos->reference};

    my @readings_off;
    if( $old_state && $old_state eq $rname ) {
	# Turn off the node. We turn on no others by default.
	push( @readings_off, $reading );
    } else {
	# Turn on the node.
	$self->lemmata->{$pos->reference} = $rname;
	# Any other 'on' readings in the same position should be off
	# if we have a fixed position.
	push( @readings_off, $self->same_position_as( $reading, 1 ) )
	    if $pos->fixed;
	# Any node that is an identical transposed one should be off.
	push( @readings_off, $reading->identical_readings );
    }
    @readings_off = unique_list( @readings_off );
	
    # Turn off the readings that need to be turned off.
    my @readings_delemmatized;
    foreach my $n ( @readings_off ) {
	my $npos = $n->position;
	my $state = undef;
	$state = $self->lemmata->{$npos->reference}
	    if defined $self->lemmata->{$npos->reference};
	if( $state && $state eq $n->name ) { 
	    # this reading is still on, so turn it off
	    push( @readings_delemmatized, $n );
	    my $new_state = undef;
	    if( $npos->fixed && $n eq $reading ) {
		# This is the reading that was clicked, so if there are no
		# other readings there and this is a fixed position, turn off 
		# the position.  In all other cases, restore the ellipsis.
		my @other_n = $self->same_position_as( $n ); # TODO do we need strict?
		$new_state = 0 unless @other_n;
	    }
	    $self->lemmata->{$npos->reference} = $new_state;
	} elsif( $old_state && $old_state eq $n->name ) { 
	    # another reading has already been turned on here
	    push( @readings_delemmatized, $n );
	} # else some other reading was on anyway, so pass.
    }
    return @readings_delemmatized;
}

sub same_position_as {
    my( $self, $reading, $strict ) = @_;
    my $pos = $reading->position;
    my %onpath = ( $reading->name => 1 );
    # TODO This might not always be sufficient.  We really want to
    # exclude all readings on this one's path between its two
    # common points.
    map { $onpath{$_->name} = 1 } $reading->neighbor_readings;
    my @same = grep { !$onpath{$_->name} } 
        $self->readings_at_position( $reading->position, $strict );
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

sub add_hash_entry {
    my( $hash, $key, $entry ) = @_;
    if( exists $hash->{$key} ) {
	push( @{$hash->{$key}}, $entry );
    } else {
	$hash->{$key} = [ $entry ];
    }
}

no Moose;
__PACKAGE__->meta->make_immutable;
