package Text::Tradition::Stemma;

use Bio::Phylo::IO;
use Encode qw( decode_utf8 );
use File::chdir;
use File::Temp;
use Graph;
use Graph::Convert;
use Graph::Reader::Dot;
use IPC::Run qw/ run binary /;
use Moose;
use Text::Balanced qw/ extract_bracketed /;

has collation => (
    is => 'ro',
    isa => 'Text::Tradition::Collation',
    required => 1,
    );  

# TODO Think about making a new class for the graphs, which has apsp as a property.
has graph => (
    is => 'rw',
    isa => 'Graph',
    predicate => 'has_graph',
    );
    
has apsp => (
    is => 'ro',
    isa => 'Graph',
    writer => '_save_apsp',
    );
    
has distance_trees => (
    is => 'ro',
    isa => 'ArrayRef[Graph]',
    writer => '_save_distance_trees',
    predicate => 'has_distance_trees',
    );
    
has distance_apsps => (
    is => 'ro',
    isa => 'ArrayRef[Graph]',
    writer => '_save_distance_apsps',
    );
        
sub BUILD {
    my( $self, $args ) = @_;
    # If we have been handed a dotfile, initialize it into a graph.
    if( exists $args->{'dot'} ) {
        # Open the file, assume UTF-8
        open( my $dot, $args->{'dot'} ) or warn "Failed to read dot file";
        # TODO don't bother if we haven't opened
        binmode $dot, ":utf8";
        my $reader = Graph::Reader::Dot->new();
        my $graph = $reader->read_graph( $dot );
        $graph 
            ? $self->graph( $graph ) 
            : warn "Failed to parse dot file " . $args->{'dot'};
    }
}

# If we are saving a new graph, calculate its apsp values.
after 'graph' => sub {
    my( $self, $args ) = @_;
    if( $args ) {
        # We had a new graph.
        my $undirected;
        if( $self->graph->is_directed ) {
            # Make an undirected version.
            $undirected = Graph->new( 'undirected' => 1 );
            foreach my $v ( $self->graph->vertices ) {
                $undirected->add_vertex( $v );
            }
            foreach my $e ( $self->graph->edges ) {
                $undirected->add_edge( @$e );
            }
        } else {
            $undirected = $self->graph;
        }
        $self->_save_apsp( $undirected->APSP_Floyd_Warshall() );
    }       
};

# Render the stemma as SVG.
sub as_svg {
    my( $self, $opts ) = @_;
    # TODO add options for display, someday
    my $dgraph = Graph::Convert->as_graph_easy( $self->graph );
    # Set some class display attributes for 'hypothetical' and 'extant' nodes
    $dgraph->set_attribute( 'flow', 'south' );
    foreach my $n ( $dgraph->nodes ) {
        if( $n->attribute( 'class' ) eq 'hypothetical' ) {
            $n->set_attribute( 'shape', 'point' );
            $n->set_attribute( 'pointshape', 'diamond' );
        } else {
            $n->set_attribute( 'shape', 'ellipse' );
        }
    }
    
    # Render to svg via graphviz
    my @lines = split( /\n/, $dgraph->as_graphviz() );
    # Add the size attribute
    if( $opts->{'size'} ) {
        my $sizeline = "  graph [ size=\"" . $opts->{'size'} . "\" ]";
        splice( @lines, 1, 0, $sizeline );
    }
    my @cmd = qw/dot -Tsvg/;
    my( $svg, $err );
    my $dotfile = File::Temp->new();
    ## TODO REMOVE
    # $dotfile->unlink_on_destroy(0);
    binmode $dotfile, ':utf8';
    print $dotfile join( "\n", @lines );
    push( @cmd, $dotfile->filename );
    run( \@cmd, ">", binary(), \$svg );
    $svg = decode_utf8( $svg );
    return $svg;
}

#### Methods for calculating phylogenetic trees ####

before 'distance_trees' => sub {
    my $self = shift;
    my %args = @_;
    # TODO allow specification of method for calculating distance tree
    if( $args{'recalc'} || !$self->has_distance_trees ) {
        # We need to make a tree before we can return it.
        my( $ok, $result ) = $self->run_phylip_pars();
        if( $ok ) {
            # Save the resulting trees
            my $trees = _parse_newick( $result );
            $self->_save_distance_trees( $trees );
            # and calculate their APSP values.
            my @apsps;
            foreach my $t ( @$trees ) {
                push( @apsps, $t->APSP_Floyd_Warshall() );
            }
            $self->_save_distance_apsps( \@apsps );
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
    my $ctr = 0;
    foreach my $word ( @$row ) {
        if( $word && !exists $unique{$word} ) {
            $unique{$word} = chr( 65 + $ctr );
            $ctr++;
        }
    }
    if( scalar( keys %unique ) > 8 ) {
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
    ### HACKY HACKY
    my $PHYLIP_PATH = '/Users/tla/Projects/phylip-3.69/exe';
    my $program = "pars";
    if( $^O eq 'darwin' ) {
        $program = "$PHYLIP_PATH/$program.app/Contents/MacOS/$program";
    } else {
        $program = "$PHYLIP_PATH/$program";
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
