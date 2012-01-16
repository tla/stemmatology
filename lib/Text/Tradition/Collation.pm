package Text::Tradition::Collation;

use Encode qw( decode_utf8 );
use File::Temp;
use Graph;
use IPC::Run qw( run binary );
use Text::CSV_XS;
use Text::Tradition::Collation::Reading;
use Text::Tradition::Collation::RelationshipStore;
use XML::LibXML;
use Moose;

has 'sequence' => (
    is => 'ro',
    isa => 'Graph',
    default => sub { Graph->new() },
    handles => {
    	paths => 'edges',
    },
    );
    
has 'relations' => (
	is => 'ro',
	isa => 'Text::Tradition::Collation::RelationshipStore',
	handles => {
		relationships => 'relationships',
		related_readings => 'related_readings',
	},
	writer => '_set_relations',
	);

has 'tradition' => (
    is => 'ro',
    isa => 'Text::Tradition',
    weak_ref => 1,
    );

has 'readings' => (
	isa => 'HashRef[Text::Tradition::Collation::Reading]',
	traits => ['Hash'],
    handles => {
        reading     => 'get',
        _add_reading => 'set',
        del_reading => 'delete',
        has_reading => 'exists',
        readings   => 'values',
    },
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
    
has 'start' => (
	is => 'ro',
	isa => 'Text::Tradition::Collation::Reading',
	writer => '_set_start',
	weak_ref => 1,
	);

has 'end' => (
	is => 'ro',
	isa => 'Text::Tradition::Collation::Reading',
	writer => '_set_end',
	weak_ref => 1,
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
    my $self = shift;
    $self->_set_relations( Text::Tradition::Collation::RelationshipStore->new( 'collation' => $self ) );
    $self->_set_start( $self->add_reading( { 'collation' => $self, 'is_start' => 1 } ) );
    $self->_set_end( $self->add_reading( { 'collation' => $self, 'is_end' => 1 } ) );
}

### Reading construct/destruct functions

sub add_reading {
	my( $self, $reading ) = @_;
	unless( ref( $reading ) eq 'Text::Tradition::Collation::Reading' ) {
		my %args = %$reading;
		$reading = Text::Tradition::Collation::Reading->new( 
			'collation' => $self,
			%args );
	}
	# First check to see if a reading with this ID exists.
	if( $self->reading( $reading->id ) ) {
		warn "Collation already has a reading with id " . $reading->id;
		return undef;
	}
	$self->_add_reading( $reading->id => $reading );
	# Once the reading has been added, put it in both graphs.
	$self->sequence->add_vertex( $reading->id );
	$self->relations->add_reading( $reading->id );
	return $reading;
};

around del_reading => sub {
	my $orig = shift;
	my $self = shift;
	my $arg = shift;
	
	if( ref( $arg ) eq 'Text::Tradition::Collation::Reading' ) {
		$arg = $arg->id;
	}
	# Remove the reading from the graphs.
	$self->sequence->delete_vertex( $arg );
	$self->relations->delete_reading( $arg );
	
	# Carry on.
	$self->$orig( $arg );
};

# merge_readings( $main, $to_be_deleted );

sub merge_readings {
	my $self = shift;

	# We only need the IDs for adding paths to the graph, not the reading
	# objects themselves.
    my( $kept, $deleted, $combine_char ) = $self->_stringify_args( @_ );

    # The kept reading should inherit the paths and the relationships
    # of the deleted reading.
	foreach my $path ( $self->sequence->edges_at( $deleted ) ) {
		my @vector = ( $kept );
		push( @vector, $path->[1] ) if $path->[0] eq $deleted;
		unshift( @vector, $path->[0] ) if $path->[1] eq $deleted;
		next if $vector[0] eq $vector[1]; # Don't add a self loop
		my %wits = %{$self->sequence->get_edge_attributes( @$path )};
		$self->sequence->add_edge( @vector );
		my $fwits = $self->sequence->get_edge_attributes( @vector );
		@wits{keys %$fwits} = values %$fwits;
		$self->sequence->set_edge_attributes( @vector, \%wits );
	}
	$self->relations->merge_readings( $kept, $deleted, $combine_char );
	
	# Do the deletion deed.
	if( $combine_char ) {
		my $kept_obj = $self->reading( $kept );
		my $new_text = join( $combine_char, $kept_obj->text, 
			$self->reading( $deleted )->text );
		$kept_obj->alter_text( $new_text );
	}
	$self->del_reading( $deleted );
}


# Helper function for manipulating the graph.
sub _stringify_args {
	my( $self, $first, $second, $arg ) = @_;
    $first = $first->id
        if ref( $first ) eq 'Text::Tradition::Collation::Reading';
    $second = $second->id
        if ref( $second ) eq 'Text::Tradition::Collation::Reading';        
    return( $first, $second, $arg );
}

### Path logic

sub add_path {
	my $self = shift;

	# We only need the IDs for adding paths to the graph, not the reading
	# objects themselves.
    my( $source, $target, $wit ) = $self->_stringify_args( @_ );

	# Connect the readings
    $self->sequence->add_edge( $source, $target );
    # Note the witness in question
    $self->sequence->set_edge_attribute( $source, $target, $wit, 1 );
};

sub del_path {
	my $self = shift;
	my @args;
	if( ref( $_[0] ) eq 'ARRAY' ) {
		my $e = shift @_;
		@args = ( @$e, @_ );
	} else {
		@args = @_;
	}

	# We only need the IDs for adding paths to the graph, not the reading
	# objects themselves.
    my( $source, $target, $wit ) = $self->_stringify_args( @args );

	if( $self->sequence->has_edge_attribute( $source, $target, $wit ) ) {
		$self->sequence->delete_edge_attribute( $source, $target, $wit );
	}
	unless( keys %{$self->sequence->get_edge_attributes( $source, $target )} ) {
		$self->sequence->delete_edge( $source, $target );
	}
}


# Extra graph-alike utility
sub has_path {
	my $self = shift;
    my( $source, $target, $wit ) = $self->_stringify_args( @_ );
	return undef unless $self->sequence->has_edge( $source, $target );
	return $self->sequence->has_edge_attribute( $source, $target, $wit );
}

=head2 add_relationship( $reading1, $reading2, $definition )

Adds the specified relationship between the two readings.  A relationship
is transitive (i.e. undirected); the options for its definition may be found
in Text::Tradition::Collation::Relationship.

=cut

# Wouldn't it be lovely if edges could be objects, and all this type checking
# and attribute management could be done via Moose?

sub add_relationship {
	my $self = shift;
    my( $source, $target, $opts ) = $self->_stringify_args( @_ );
    my( $ret, @vectors ) = $self->relations->add_relationship( $source, 
    	$self->reading( $source ), $target, $self->reading( $target ), $opts );
    # Force a full rank recalculation every time. Yuck.
    $self->calculate_ranks() if $ret && $self->end->has_rank;
    return( $ret, @vectors );
}

=head2 reading_witnesses( $reading )

Return a list of sigils corresponding to the witnesses in which the reading appears.

=cut

sub reading_witnesses {
	my( $self, $reading ) = @_;
	# We need only check either the incoming or the outgoing edges; I have
	# arbitrarily chosen "incoming".  Thus, special-case the start node.
	if( $reading eq $self->start ) {
		return map { $_->sigil } $self->tradition->witnesses;
	}
	my %all_witnesses;
	foreach my $e ( $self->sequence->edges_to( $reading ) ) {
		my $wits = $self->sequence->get_edge_attributes( @$e );
		@all_witnesses{ keys %$wits } = 1;
	}
	return keys %all_witnesses;
}

=head2 Output method(s)

=over

=item B<as_svg>

print $graph->as_svg( $recalculate );

Returns an SVG string that represents the graph, via as_dot and graphviz.

=cut

sub as_svg {
    my( $self ) = @_;
        
    my @cmd = qw/dot -Tsvg/;
    my( $svg, $err );
    my $dotfile = File::Temp->new();
    ## TODO REMOVE
    # $dotfile->unlink_on_destroy(0);
    binmode $dotfile, ':utf8';
    print $dotfile $self->as_dot();
    push( @cmd, $dotfile->filename );
    run( \@cmd, ">", binary(), \$svg );
    $svg = decode_utf8( $svg );
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
    $view = 'sequence' unless $view;
    # TODO consider making some of these things configurable
    my $graph_name = $self->tradition->name;
    $graph_name =~ s/[^\w\s]//g;
    $graph_name = join( '_', split( /\s+/, $graph_name ) );
    my $dot = sprintf( "digraph %s {\n", $graph_name );
    $dot .= "\tedge [ arrowhead=open ];\n";
    $dot .= "\tgraph [ rankdir=LR,bgcolor=none ];\n";
    $dot .= sprintf( "\tnode [ fontsize=%d, fillcolor=%s, style=%s, shape=%s ];\n",
                     11, "white", "filled", "ellipse" );

    foreach my $reading ( $self->readings ) {
        # Need not output nodes without separate labels
        next if $reading->id eq $reading->text;
        my $label = $reading->text;
        $label =~ s/\"/\\\"/g;
        $dot .= sprintf( "\t\"%s\" [ label=\"%s\" ];\n", $reading->id, $label );
    }
    
    # TODO do something sensible for relationships

    my @edges = $self->paths;
    foreach my $edge ( @edges ) {
        my %variables = ( 'color' => '#000000',
                          'fontcolor' => '#000000',
                          'label' => join( ', ', $self->path_display_label( $edge ) ),
            );
        my $varopts = join( ', ', map { $_.'="'.$variables{$_}.'"' } sort keys %variables );
        # Account for the rank gap if necessary
        my $rankgap = $self->reading( $edge->[1] )->rank 
        	- $self->reading( $edge->[0] )->rank;
		$varopts .= ", minlen=$rankgap" if $rankgap > 1;
        $dot .= sprintf( "\t\"%s\" -> \"%s\" [ %s ];\n",
                         $edge->[0], $edge->[1], $varopts );
    }
    $dot .= "}\n";
    return $dot;
}

sub path_witnesses {
	my( $self, @edge ) = @_;
	# If edge is an arrayref, cope.
	if( @edge == 1 && ref( $edge[0] ) eq 'ARRAY' ) {
		my $e = shift @edge;
		@edge = @$e;
	}
	my @wits = keys %{$self->sequence->get_edge_attributes( @edge )};
	return sort @wits;
}

sub path_display_label {
	my( $self, $edge ) = @_;
	my @wits = $self->path_witnesses( $edge );
	my $maj = scalar( $self->tradition->witnesses ) * 0.6;
	if( scalar @wits > $maj ) {
		return 'majority';
	} else {
		return join( ', ', @wits );
	}
}
		

=item B<as_graphml>

print $graph->as_graphml( $recalculate )

Returns a GraphML representation of the collation graph, with
transposition information and position information. Unless
$recalculate is passed (and is a true value), the method will return a
cached copy of the SVG after the first call to the method.

=cut

sub as_graphml {
    my( $self ) = @_;

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

    # Add the data keys for the graph
    my %graph_data_keys;
    my $gdi = 0;
    my @graph_attributes = qw/ version wit_list_separator baselabel linear ac_label /;
    foreach my $datum ( @graph_attributes ) {
    	$graph_data_keys{$datum} = 'dg'.$gdi++;
        my $key = $root->addNewChild( $graphml_ns, 'key' );
        $key->setAttribute( 'attr.name', $datum );
        $key->setAttribute( 'attr.type', $key eq 'linear' ? 'boolean' : 'string' );
        $key->setAttribute( 'for', 'graph' );
        $key->setAttribute( 'id', $graph_data_keys{$datum} );    	
    }

    # Add the data keys for nodes
    my %node_data_keys;
    my $ndi = 0;
    my %node_data = ( 
    	id => 'string',
    	text => 'string',
    	rank => 'string',
    	is_start => 'boolean',
    	is_end => 'boolean',
    	is_lacuna => 'boolean',
    	);
    foreach my $datum ( keys %node_data ) {
        $node_data_keys{$datum} = 'dn'.$ndi++;
        my $key = $root->addNewChild( $graphml_ns, 'key' );
        $key->setAttribute( 'attr.name', $datum );
        $key->setAttribute( 'attr.type', $node_data{$datum} );
        $key->setAttribute( 'for', 'node' );
        $key->setAttribute( 'id', $node_data_keys{$datum} );
    }

    # Add the data keys for edges, i.e. witnesses
    my $edi = 0;
    my %edge_data_keys;
    my %edge_data = (
    	class => 'string',				# Class, deprecated soon
    	witness => 'string',			# ID/label for a path
    	relationship => 'string',		# ID/label for a relationship
    	extra => 'boolean',				# Path key
    	colocated => 'boolean',			# Relationship key
    	non_correctable => 'boolean',	# Relationship key
    	non_independent => 'boolean',	# Relationship key
    	);
    foreach my $datum ( keys %edge_data ) {
        $edge_data_keys{$datum} = 'de'.$edi++;
        my $key = $root->addNewChild( $graphml_ns, 'key' );
        $key->setAttribute( 'attr.name', $datum );
        $key->setAttribute( 'attr.type', $edge_data{$datum} );
        $key->setAttribute( 'for', 'edge' );
        $key->setAttribute( 'id', $edge_data_keys{$datum} );
    }

    # Add the collation graph itself
    my $sgraph = $root->addNewChild( $graphml_ns, 'graph' );
    $sgraph->setAttribute( 'edgedefault', 'directed' );
    $sgraph->setAttribute( 'id', $self->tradition->name );
    $sgraph->setAttribute( 'parse.edgeids', 'canonical' );
    $sgraph->setAttribute( 'parse.edges', scalar($self->paths) );
    $sgraph->setAttribute( 'parse.nodeids', 'canonical' );
    $sgraph->setAttribute( 'parse.nodes', scalar($self->readings) );
    $sgraph->setAttribute( 'parse.order', 'nodesfirst' );
    	    
    # Collation attribute data
    foreach my $datum ( @graph_attributes ) {
    	my $value = $datum eq 'version' ? '3.0' : $self->$datum;
		_add_graphml_data( $sgraph, $graph_data_keys{$datum}, $value );
	}

    my $node_ctr = 0;
    my %node_hash;
    # Add our readings to the graph
    foreach my $n ( sort { $a->id cmp $b->id } $self->readings ) {
    	# Add to the main graph
        my $node_el = $sgraph->addNewChild( $graphml_ns, 'node' );
        my $node_xmlid = 'n' . $node_ctr++;
        $node_hash{ $n->id } = $node_xmlid;
        $node_el->setAttribute( 'id', $node_xmlid );
        foreach my $d ( keys %node_data ) {
        	my $nval = $n->$d;
        	_add_graphml_data( $node_el, $node_data_keys{$d}, $nval )
        		if defined $nval;
        }
    }

    # Add the path edges to the sequence graph
    my $edge_ctr = 0;
    foreach my $e ( sort { $a->[0] cmp $b->[0] } $self->sequence->edges() ) {
    	# We add an edge in the graphml for every witness in $e.
    	foreach my $wit ( $self->path_witnesses( $e ) ) {
			my( $id, $from, $to ) = ( 'e'.$edge_ctr++,
										$node_hash{ $e->[0] },
										$node_hash{ $e->[1] } );
			my $edge_el = $sgraph->addNewChild( $graphml_ns, 'edge' );
			$edge_el->setAttribute( 'source', $from );
			$edge_el->setAttribute( 'target', $to );
			$edge_el->setAttribute( 'id', $id );
			
			# It's a witness path, so add the witness
			my $base = $wit;
			my $key = $edge_data_keys{'witness'};
			# Is this an ante-corr witness?
			my $aclabel = $self->ac_label;
			if( $wit =~ /^(.*)\Q$aclabel\E$/ ) {
				# Keep the base witness
				$base = $1;
				# ...and record that this is an 'extra' reading path
				_add_graphml_data( $edge_el, $edge_data_keys{'extra'}, $aclabel );
			}
			_add_graphml_data( $edge_el, $edge_data_keys{'witness'}, $base );
			_add_graphml_data( $edge_el, $edge_data_keys{'class'}, 'path' );
		}
	}
	
	# Add the relationship graph to the XML
	$self->relations->as_graphml( $root );

    # Save and return the thing
    my $result = decode_utf8( $graphml->toString(1) );
    return $result;
}

sub _add_graphml_data {
    my( $el, $key, $value ) = @_;
    return unless defined $value;
    my $data_el = $el->addNewChild( $el->namespaceURI, 'data' );
    $data_el->setAttribute( 'key', $key );
    $data_el->appendText( $value );
}

=item B<as_csv>

print $graph->as_csv( $recalculate )

Returns a CSV alignment table representation of the collation graph, one
row per witness (or witness uncorrected.) 

=cut

sub as_csv {
    my( $self ) = @_;
    my $table = $self->make_alignment_table;
    my $csv = Text::CSV_XS->new( { binary => 1, quote_null => 0 } );    
    my @result;
    # Make the header row
    $csv->combine( map { $_->{'witness'} } @{$table->{'alignment'}} );
	push( @result, decode_utf8( $csv->string ) );
    # Make the rest of the rows
    foreach my $idx ( 0 .. $table->{'length'} - 1 ) {
    	my @rowobjs = map { $_->{'tokens'}->[$idx] } @{$table->{'alignment'}};
    	my @row = map { $_ ? $_->{'t'} : $_ } @rowobjs;
        $csv->combine( @row );
        push( @result, decode_utf8( $csv->string ) );
    }
    return join( "\n", @result );
}

=item B<make_alignment_table>

my $table = $graph->make_alignment_table( $use_refs, \@wits_to_include )

Return a reference to an alignment table, in a slightly enhanced CollateX
format which looks like this:

 $table = { alignment => [ { witness => "SIGIL", 
                             tokens => [ { t => "READINGTEXT" }, ... ] },
                           { witness => "SIG2", 
                             tokens => [ { t => "READINGTEXT" }, ... ] },
                           ... ],
            length => TEXTLEN };

If $use_refs is set to 1, the reading object is returned in the table 
instead of READINGTEXT; if not, the text of the reading is returned.
If $wits_to_include is set to a hashref, only the witnesses whose sigil
keys have a true hash value will be included.

=cut

sub make_alignment_table {
    my( $self, $noderefs, $include ) = @_;
    unless( $self->linear ) {
        warn "Need a linear graph in order to make an alignment table";
        return;
    }
    my $table = { 'alignment' => [], 'length' => $self->end->rank - 1 };
    my @all_pos = ( 1 .. $self->end->rank - 1 );
    foreach my $wit ( sort { $a->sigil cmp $b->sigil } $self->tradition->witnesses ) {
    	if( $include ) {
    		next unless $include->{$wit->sigil};
    	}
        # print STDERR "Making witness row(s) for " . $wit->sigil . "\n";
        my @wit_path = $self->reading_sequence( $self->start, $self->end, $wit->sigil );
        my @row = _make_witness_row( \@wit_path, \@all_pos, $noderefs );
        push( @{$table->{'alignment'}}, 
        	{ 'witness' => $wit->sigil, 'tokens' => \@row } );
        if( $wit->is_layered ) {
        	my @wit_ac_path = $self->reading_sequence( $self->start, $self->end, 
        		$wit->sigil.$self->ac_label, $wit->sigil );
            my @ac_row = _make_witness_row( \@wit_ac_path, \@all_pos, $noderefs );
			push( @{$table->{'alignment'}},
				{ 'witness' => $wit->sigil.$self->ac_label, 'tokens' => \@ac_row } );
        }           
    }
	return $table;
}

sub _make_witness_row {
    my( $path, $positions, $noderefs ) = @_;
    my %char_hash;
    map { $char_hash{$_} = undef } @$positions;
    my $debug = 0;
    foreach my $rdg ( @$path ) {
        my $rtext = $rdg->text;
        $rtext = '#LACUNA#' if $rdg->is_lacuna;
        print STDERR "rank " . $rdg->rank . "\n" if $debug;
        # print STDERR "No rank for " . $rdg->id . "\n" unless defined $rdg->rank;
        $char_hash{$rdg->rank} = $noderefs ? { 't' => $rdg } 
        								   : { 't' => $rtext };
    }
    my @row = map { $char_hash{$_} } @$positions;
    # Fill in lacuna markers for undef spots in the row
    my $last_el = shift @row;
    my @filled_row = ( $last_el );
    foreach my $el ( @row ) {
        # If we are using node reference, make the lacuna node appear many times
        # in the table.  If not, use the lacuna tag.
        if( $last_el && _el_is_lacuna( $last_el ) && !defined $el ) {
            $el = $noderefs ? $last_el : { 't' => '#LACUNA#' };
        }
        push( @filled_row, $el );
        $last_el = $el;
    }
    return @filled_row;
}

# Tiny utility function to say if a table element is a lacuna
sub _el_is_lacuna {
    my $el = shift;
    return 1 if $el->{'t'} eq '#LACUNA#';
    return 1 if ref( $el->{'t'} ) eq 'Text::Tradition::Collation::Reading'
        && $el->{'t'}->is_lacuna;
    return 0;
}

# Helper to turn the witnesses along columns rather than rows.  Assumes
# equal-sized rows.
sub _turn_table {
    my( $table ) = @_;
    my $result = [];
    return $result unless scalar @$table;
    my $nrows = scalar @{$table->[0]};
    foreach my $idx ( 0 .. $nrows - 1 ) {
        foreach my $wit ( 0 .. $#{$table} ) {
            $result->[$idx]->[$wit] = $table->[$wit]->[$idx];
        }
    }
    return $result;        
}

=back

=head2 Navigation methods

=over

=item B<start>

my $beginning = $collation->start();

Returns the beginning of the collation, a meta-reading with label '#START#'.

=item B<end>

my $end = $collation->end();

Returns the end of the collation, a meta-reading with label '#END#'.


=item B<reading_sequence>

my @readings = $graph->reading_sequence( $first, $last, $path[, $alt_path] );

Returns the ordered list of readings, starting with $first and ending
with $last, along the given witness path.  If no path is specified,
assume that the path is that of the base text (if any.)

=cut

# TODO Think about returning some lazy-eval iterator.

sub reading_sequence {
    my( $self, $start, $end, $witness, $backup ) = @_;

    $witness = $self->baselabel unless $witness;
    my @readings = ( $start );
    my %seen;
    my $n = $start;
    while( $n && $n->id ne $end->id ) {
        if( exists( $seen{$n->id} ) ) {
            warn "Detected loop at " . $n->id;
            last;
        }
        $seen{$n->id} = 1;
        
        my $next = $self->next_reading( $n, $witness, $backup );
        unless( $next ) {
            warn "Did not find any path for $witness from reading " . $n->id;
            last;
        }
        push( @readings, $next );
        $n = $next;
    }
    # Check that the last reading is our end reading.
    my $last = $readings[$#readings];
    warn "Last reading found from " . $start->text .
        " for witness $witness is not the end!"
        unless $last->id eq $end->id;
    
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
    my $answer = $self->_find_linked_reading( 'next', @_ );
	return undef unless $answer;
    return $self->reading( $answer );
}

=item B<prior_reading>

my $prior_reading = $graph->prior_reading( $reading, $witpath );

Returns the reading that precedes the given reading along the given witness
path.  

=cut

sub prior_reading {
    # Return the predecessor via the corresponding path.
    my $self = shift;
    my $answer = $self->_find_linked_reading( 'prior', @_ );
    return $self->reading( $answer );
}

sub _find_linked_reading {
    my( $self, $direction, $node, $path, $alt_path ) = @_;
    my @linked_paths = $direction eq 'next' 
        ? $self->sequence->edges_from( $node ) 
        : $self->sequence->edges_to( $node );
    return undef unless scalar( @linked_paths );
    
    # We have to find the linked path that contains all of the
    # witnesses supplied in $path.
    my( @path_wits, @alt_path_wits );
    @path_wits = sort( $self->witnesses_of_label( $path ) ) if $path;
    @alt_path_wits = sort( $self->witnesses_of_label( $alt_path ) ) if $alt_path;
    my $base_le;
    my $alt_le;
    foreach my $le ( @linked_paths ) {
        if( $self->sequence->has_edge_attribute( @$le, $self->baselabel ) ) {
            $base_le = $le;
        }
		my @le_wits = $self->path_witnesses( $le );
		if( _is_within( \@path_wits, \@le_wits ) ) {
			# This is the right path.
			return $direction eq 'next' ? $le->[1] : $le->[0];
		} elsif( _is_within( \@alt_path_wits, \@le_wits ) ) {
			$alt_le = $le;
		}
    }
    # Got this far? Return the alternate path if it exists.
    return $direction eq 'next' ? $alt_le->[1] : $alt_le->[0]
        if $alt_le;

    # Got this far? Return the base path if it exists.
    return $direction eq 'next' ? $base_le->[1] : $base_le->[0]
        if $base_le;

    # Got this far? We have no appropriate path.
    warn "Could not find $direction node from " . $node->id 
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

# For use when a collation is constructed from a base text and an apparatus.
# We have the sequences of readings and just need to add path edges.
# When we are done, clear out the witness path attributes, as they are no
# longer needed.
# TODO Find a way to replace the witness path attributes with encapsulated functions?

sub make_witness_paths {
    my( $self ) = @_;
    foreach my $wit ( $self->tradition->witnesses ) {
        # print STDERR "Making path for " . $wit->sigil . "\n";
        $self->make_witness_path( $wit );
    }
}

sub make_witness_path {
    my( $self, $wit ) = @_;
    my @chain = @{$wit->path};
    my $sig = $wit->sigil;
    foreach my $idx ( 0 .. $#chain-1 ) {
        $self->add_path( $chain[$idx], $chain[$idx+1], $sig );
    }
    if( $wit->is_layered ) {
        @chain = @{$wit->uncorrected_path};
        foreach my $idx( 0 .. $#chain-1 ) {
            my $source = $chain[$idx];
            my $target = $chain[$idx+1];
            $self->add_path( $source, $target, $sig.$self->ac_label )
                unless $self->has_path( $source, $target, $sig );
        }
    }
    $wit->clear_path;
    $wit->clear_uncorrected_path;
}

sub calculate_ranks {
    my $self = shift;
    # Walk a version of the graph where every node linked by a relationship 
    # edge is fundamentally the same node, and do a topological ranking on
    # the nodes in this graph.
    my $topo_graph = Graph->new();
    my %rel_containers;
    my $rel_ctr = 0;
    # Add the nodes
    foreach my $r ( $self->readings ) {
        next if exists $rel_containers{$r->id};
        my @rels = $r->related_readings( 'colocated' );
        if( @rels ) {
            # Make a relationship container.
            push( @rels, $r );
            my $rn = 'rel_container_' . $rel_ctr++;
            $topo_graph->add_vertex( $rn );
            foreach( @rels ) {
                $rel_containers{$_->id} = $rn;
            }
        } else {
            # Add a new node to mirror the old node.
            $rel_containers{$r->id} = $r->id;
            $topo_graph->add_vertex( $r->id );
        }
    }

    # Add the edges.
    foreach my $r ( $self->readings ) {
        foreach my $n ( $self->sequence->successors( $r->id ) ) {
        	my( $tfrom, $tto ) = ( $rel_containers{$r->id},
        		$rel_containers{$n} );
        	$DB::single = 1 unless $tfrom && $tto;
            $topo_graph->add_edge( $tfrom, $tto );
        }
    }
    
    # Now do the rankings, starting with the start node.
    my $topo_start = $rel_containers{$self->start->id};
    my $node_ranks = { $topo_start => 0 };
    my @curr_origin = ( $topo_start );
    # A little iterative function.
    while( @curr_origin ) {
        @curr_origin = _assign_rank( $topo_graph, $node_ranks, @curr_origin );
    }
    # Transfer our rankings from the topological graph to the real one.
    foreach my $r ( $self->readings ) {
        if( defined $node_ranks->{$rel_containers{$r->id}} ) {
            $r->rank( $node_ranks->{$rel_containers{$r->id}} );
        } else {
            $DB::single = 1;
            die "No rank calculated for node " . $r->id 
                . " - do you have a cycle in the graph?";
        }
    }
}

sub _assign_rank {
    my( $graph, $node_ranks, @current_nodes ) = @_;
    # Look at each of the children of @current_nodes.  If all the child's 
    # parents have a rank, assign it the highest rank + 1 and add it to 
    # @next_nodes.  Otherwise skip it; we will return when the highest-ranked
    # parent gets a rank.
    my @next_nodes;
    foreach my $c ( @current_nodes ) {
        warn "Current reading $c has no rank!"
            unless exists $node_ranks->{$c};
        # print STDERR "Looking at child of node $c, rank " 
        #     . $node_ranks->{$c} . "\n";
        foreach my $child ( $graph->successors( $c ) ) {
            next if exists $node_ranks->{$child};
            my $highest_rank = -1;
            my $skip = 0;
            foreach my $parent ( $graph->predecessors( $child ) ) {
                if( exists $node_ranks->{$parent} ) {
                    $highest_rank = $node_ranks->{$parent} 
                        if $highest_rank <= $node_ranks->{$parent};
                } else {
                    $skip = 1;
                    last;
                }
            }
            next if $skip;
            my $c_rank = $highest_rank + 1;
            # print STDERR "Assigning rank $c_rank to node $child \n";
            $node_ranks->{$child} = $c_rank;
            push( @next_nodes, $child );
        }
    }
    return @next_nodes;
}

# Another method to make up for rough collation methods.  If the same reading
# appears multiple times at the same rank, collapse the nodes.
sub flatten_ranks {
    my $self = shift;
    my %unique_rank_rdg;
    foreach my $rdg ( $self->readings ) {
        next unless $rdg->has_rank;
        my $key = $rdg->rank . "||" . $rdg->text;
        if( exists $unique_rank_rdg{$key} ) {
            # Combine!
            # print STDERR "Combining readings at same rank: $key\n";
            $self->merge_readings( $unique_rank_rdg{$key}, $rdg );
        } else {
            $unique_rank_rdg{$key} = $rdg;
        }
    }
}


## Utility functions
    
# Return the string that joins together a list of witnesses for
# display on a single path.
sub witnesses_of_label {
    my( $self, $label ) = @_;
    my $regex = $self->wit_list_separator;
    my @answer = split( /\Q$regex\E/, $label );
    return @answer;
}    

=begin testing

use Text::Tradition;

my $cxfile = 't/data/Collatex-16.xml';
my $t = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'CollateX',
    'file'  => $cxfile,
    );
my $c = $t->collation;

is( $c->common_predecessor( $c->reading('n9'), $c->reading('n23') )->id, 
    'n20', "Found correct common predecessor" );
is( $c->common_successor( $c->reading('n9'), $c->reading('n23') )->id, 
    '#END#', "Found correct common successor" );

is( $c->common_predecessor( $c->reading('n19'), $c->reading('n17') )->id, 
    'n16', "Found correct common predecessor for readings on same path" );
is( $c->common_successor( $c->reading('n21'), $c->reading('n26') )->id, 
    '#END#', "Found correct common successor for readings on same path" );

=end testing

=cut

## Return the closest reading that is a predecessor of both the given readings.
sub common_predecessor {
	my $self = shift;
	return $self->common_in_path( @_, 'predecessors' );
}

sub common_successor {
	my $self = shift;
	return $self->common_in_path( @_, 'successors' );
}

sub common_in_path {
	my( $self, $r1, $r2, $dir ) = @_;
	my $iter = $r1->rank > $r2->rank ? $r1->rank : $r2->rank;
	$iter = $self->end->rank - $iter if $dir eq 'successors';
	my @candidates;
	my @last_checked = ( $r1, $r2 );
	my %all_seen;
	while( !@candidates ) {
		my @new_lc;
		foreach my $lc ( @last_checked ) {
			foreach my $p ( $lc->$dir ) {
				if( $all_seen{$p->id} ) {
					push( @candidates, $p );
				} else {
					$all_seen{$p->id} = 1;
					push( @new_lc, $p );
				}
			}
		}
		@last_checked = @new_lc;
	}
	my @answer = sort { $a->rank <=> $b->rank } @candidates;
	return $dir eq 'predecessors' ? pop( @answer ) : shift ( @answer );
}

no Moose;
__PACKAGE__->meta->make_immutable;

=head1 BUGS / TODO

=over

=item * Think about making Relationship objects again

=back
