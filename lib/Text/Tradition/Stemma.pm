package Text::Tradition::Stemma;

use Bio::Phylo::IO;
use Encode qw( decode_utf8 );
use File::chdir;
use File::Temp;
use File::Which;
use Graph;
use Graph::Reader::Dot;
use IPC::Run qw/ run binary /;
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
	# Assume utf-8
	binmode( $dotfh, ':utf8' );
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
		warn "Failed to parse dot in $dotfh";
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
            my $trees = _parse_newick( $result );
            $self->_save_distance_trees( $trees );
            $self->distance_program( $args{'program'} );
        } else {
            warn "Failed to calculate distance trees: $result";
        }
    }
};
        
sub make_character_matrix {
    my $self = shift;
    unless( $self->collation->linear ) {
        warn "Need a linear graph in order to make an alignment table";
        return;
    }
    my $table = $self->collation->make_alignment_table;
    # Push the names of the witnesses to initialize the rows of the matrix.
    my @matrix = map { [ $self->_normalize_ac( $_ ) ] } @{$table->[0]};
    foreach my $token_index ( 1 .. $#{$table} ) {
        # First implementation: make dumb alignment table, caring about
        # nothing except which reading is in which position.
        my @chars = convert_characters( $table->[$token_index] );
        foreach my $idx ( 0 .. $#matrix ) {
            push( @{$matrix[$idx]}, $chars[$idx] );
        }
    }
    return \@matrix;
} 

sub _normalize_ac {
    my( $self, $witname ) = @_;
    my $ac = $self->collation->ac_label;
    if( $witname =~ /(.*)\Q$ac\E$/ ) {
        $witname = $1 . '_ac';
    }
    return sprintf( "%-10s", $witname );
}

sub convert_characters {
    my $row = shift;
    # This is a simple algorithm that treats every reading as different.
    # Eventually we will want to be able to specify how relationships
    # affect the character matrix.
    my %unique = ( '__UNDEF__' => 'X',
                   '#LACUNA#'  => '?',
                 );
    my %count;
    my $ctr = 0;
    foreach my $word ( @$row ) {
        if( $word && !exists $unique{$word} ) {
            $unique{$word} = chr( 65 + $ctr );
            $ctr++;
        }
        $count{$word}++ if $word;
    }
    # Try to keep variants under 8 by lacunizing any singletons.
    if( scalar( keys %unique ) > 8 ) {
		foreach my $word ( keys %count ) {
			if( $count{$word} == 1 ) {
				$unique{$word} = '?';
			}
		}
    }
    my %u = reverse %unique;
    if( scalar( keys %u ) > 8 ) {
        warn "Have more than 8 variants on this location; phylip will break";
    }
    my @chars = map { $_ ? $unique{$_} : $unique{'__UNDEF__' } } @$row;
    return @chars;
}

sub phylip_pars_input {
    my $self = shift;
    my $character_matrix = $self->make_character_matrix;
    my $input = '';
    my $rows = scalar @{$character_matrix};
    my $columns = scalar @{$character_matrix->[0]} - 1;
    $input .= "\t$rows\t$columns\n";
    foreach my $row ( @{$character_matrix} ) {
        $input .= join( '', @$row ) . "\n";
    }
    return $input;
}

sub run_phylip_pars {
    my $self = shift;

    # Set up a temporary directory for all the default Phylip files.
    my $phylip_dir = File::Temp->newdir();
    # $phylip_dir->unlink_on_destroy(0);
    # We need an infile, and we need a command input file.
    open( MATRIX, ">$phylip_dir/infile" ) or die "Could not write $phylip_dir/infile";
    print MATRIX $self->phylip_pars_input();
    close MATRIX;

    open( CMD, ">$phylip_dir/cmdfile" ) or die "Could not write $phylip_dir/cmdfile";
    ## TODO any configuration parameters we want to set here
#   U                 Search for best tree?  Yes
#   S                        Search option?  More thorough search
#   V              Number of trees to save?  100
#   J     Randomize input order of species?  No. Use input order
#   O                        Outgroup root?  No, use as outgroup species 1
#   T              Use Threshold parsimony?  No, use ordinary parsimony
#   W                       Sites weighted?  No
#   M           Analyze multiple data sets?  No
#   I            Input species interleaved?  Yes
#   0   Terminal type (IBM PC, ANSI, none)?  ANSI
#   1    Print out the data at start of run  No
#   2  Print indications of progress of run  Yes
#   3                        Print out tree  Yes
#   4          Print out steps in each site  No
#   5  Print character at all nodes of tree  No
#   6       Write out trees onto tree file?  Yes
    print CMD "Y\n";
    close CMD;

    # And then we run the program.
    my $program = File::Which::which( 'pars' );
    unless( -x $program ) {
		return( undef, "Phylip pars not found in path" );
    }

    {
        # We need to run it in our temporary directory where we have created
        # all the expected files.
        local $CWD = $phylip_dir;
        my @cmd = ( $program );
        run \@cmd, '<', 'cmdfile', '>', '/dev/null';
    }
    # Now our output should be in 'outfile' and our tree in 'outtree',
    # both in the temp directory.

    my @outtree;
    if( -f "$phylip_dir/outtree" ) {
        open( TREE, "$phylip_dir/outtree" ) or die "Could not open outtree for read";
        @outtree = <TREE>;
        close TREE;
    }
    return( 1, join( '', @outtree ) ) if @outtree;

    my @error;
    if( -f "$phylip_dir/outfile" ) {
        open( OUTPUT, "$phylip_dir/outfile" ) or die "Could not open output for read";
        @error = <OUTPUT>;
        close OUTPUT;
    } else {
        push( @error, "Neither outtree nor output file was produced!" );
    }
    return( undef, join( '', @error ) );
}

sub _parse_newick {
    my $newick = shift;
    my @trees;
    # Parse the result into a tree
    my $forest = Bio::Phylo::IO->parse( 
        -format => 'newick',
        -string => $newick,
        );
    # Turn the tree into a graph, starting with the root node
    foreach my $tree ( @{$forest->get_entities} ) {
        push( @trees, _graph_from_bio( $tree ) );
    }
    return \@trees;
}

sub _graph_from_bio {
    my $tree = shift;
    my $graph = Graph->new( 'undirected' => 1 );
    # Give all the intermediate anonymous nodes a name.
    my $i = 0;
    foreach my $n ( @{$tree->get_entities} ) {
        next if $n->get_name;
        $n->set_name( $i++ );
    }
    my $root = $tree->get_root->get_name;
    $graph->add_vertex( $root );
    _add_tree_children( $graph, $root, $tree->get_root->get_children() );
    return $graph;
}

sub _add_tree_children {
    my( $graph, $parent, $tree_children ) = @_;
    foreach my $c ( @$tree_children ) {
        my $child = $c->get_name;
        $graph->add_vertex( $child );
        $graph->add_path( $parent, $child );
        _add_tree_children( $graph, $child, $c->get_children() );
    }
}

no Moose;
__PACKAGE__->meta->make_immutable;
    
1;
