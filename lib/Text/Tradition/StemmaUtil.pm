package Text::Tradition::StemmaUtil;

use strict;
use warnings;
use Exporter 'import';
use vars qw/ @EXPORT_OK /;
use Bio::Phylo::IO;
use Encode qw( decode_utf8 );
use File::chdir;
use File::Temp;
use File::Which;
use Graph;
use Graph::Reader::Dot;
use IPC::Run qw/ run binary /;
use Text::Tradition::Error;
@EXPORT_OK = qw/ make_character_matrix character_input phylip_pars 
				 parse_newick newick_to_svg /;

sub make_character_matrix {
    my( $table ) = @_;
    # Push the names of the witnesses to initialize the rows of the matrix.
    my @matrix = map { [ _normalize_witname( $_->{'witness'} ) ] } 
    				@{$table->{'alignment'}};
    foreach my $token_index ( 0 .. $table->{'length'} - 1) {
        # First implementation: make dumb alignment table, caring about
        # nothing except which reading is in which position.
        my @pos_readings = map { $_->{'tokens'}->[$token_index] }
        						@{$table->{'alignment'}};
        my @pos_text = map { $_ ? $_->{'t'} : $_ } @pos_readings;
        my @chars = convert_characters( \@pos_text );
        foreach my $idx ( 0 .. $#matrix ) {
            push( @{$matrix[$idx]}, $chars[$idx] );
        }
    }
    return \@matrix;
} 

# Helper function to make the witness name something legal for pars

sub _normalize_witname {
    my( $witname ) = @_;
    $witname =~ s/\s+/ /g;
    $witname =~ s/[\[\]\(\)\:;,]//g;
    $witname = substr( $witname, 0, 10 );
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

sub character_input {
    my $table = shift;
    my $character_matrix = make_character_matrix( $table );
    my $input = '';
    my $rows = scalar @{$character_matrix};
    my $columns = scalar @{$character_matrix->[0]} - 1;
    $input .= "\t$rows\t$columns\n";
    foreach my $row ( @{$character_matrix} ) {
        $input .= join( '', @$row ) . "\n";
    }
    return $input;
}

sub phylip_pars {
	my( $charmatrix ) = @_;
    # Set up a temporary directory for all the default Phylip files.
    my $phylip_dir = File::Temp->newdir();
    # $phylip_dir->unlink_on_destroy(0);
    # We need an infile, and we need a command input file.
    open( MATRIX, ">$phylip_dir/infile" ) or die "Could not write $phylip_dir/infile";
    print MATRIX $charmatrix;
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
		throw( "Phylip pars not found in path" );
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
    return join( '', @outtree ) if @outtree;

	# If we got this far, we are about to throw an error.
    my @error;
    if( -f "$phylip_dir/outfile" ) {
        open( OUTPUT, "$phylip_dir/outfile" ) or die "Could not open output for read";
        @error = <OUTPUT>;
        close OUTPUT;
    } else {
        push( @error, "Neither outtree nor output file was produced!" );
    }
    throw( join( '', @error ) );
}

sub parse_newick {
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

sub newick_to_svg {
	my $newick = shift;
    my $program = File::Which::which( 'figtree' );
    unless( -x $program ) {
		throw( "FigTree commandline utility not found in path" );
    }
    my $svg;
    my $nfile = File::Temp->new();
    print $nfile $newick;
    close $nfile;
	my @cmd = ( $program, '-graphic', 'SVG', $nfile );
    run( \@cmd, ">", binary(), \$svg );
    return decode_utf8( $svg );
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

sub throw {
	Text::Tradition::Error->throw( 
		'ident' => 'StemmaUtil error',
		'message' => $_[0],
		);
}

