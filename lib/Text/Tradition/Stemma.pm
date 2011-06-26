package Text::Tradition::Stemma;

use File::chdir;
use File::Temp;
use IPC::Run qw/ run /;
use Moose;
use Text::Tradition::Collation::Position;

has collation => (
    is => 'ro',
    isa => 'Text::Tradition::Collation',
    required => 1,
    );  

has character_matrix => (
    is => 'ro',
    isa => 'ArrayRef[ArrayRef[Str]]',
    writer => '_save_character_matrix',
    predicate => 'has_character_matrix',
    );

sub make_character_matrix {
    my $self = shift;
    unless( $self->collation->linear ) {
	warn "Need a linear graph in order to make an alignment table";
	return;
    }
    my @all_pos = sort { Text::Tradition::Collation::Position::str_cmp( $a, $b ) } 
        $self->collation->possible_positions;
    my $table = [];
    my $characters = {};
    map { $characters->{$_} = {} } @all_pos;
    foreach my $wit ( @{$self->collation->tradition->witnesses} ) {
	# First implementation: make dumb alignment table, caring about
	# nothing except which reading is in which position.
	push( @$table, [ $wit->sigil, make_witness_row( $characters, $wit->path, 
							\@all_pos ) ] );
	if( $wit->has_ante_corr ) {
	    push( @$table, [ $wit->sigil . "_ac", 
			     make_witness_row( $characters, $wit->uncorrected_path, 
					       \@all_pos ) ] );
	}	    
    }
    $self->_save_character_matrix( $table );
}

sub make_witness_row {
    my( $characters, $path, $positions ) = @_;
    my @row;
    my $pathdrift = 0;
    foreach my $i( 0 .. $#{$positions} ) {
	if( $path->[$i-$pathdrift]->position->minref eq $positions->[$i] ) {
	    push( @row, get_character( $path->[$i-$pathdrift], $characters ) );
	} else {
	    push( @row, 'X' );
	    $pathdrift++;
	}
	$i++;
    }
    return @row;
}
    

sub get_character {
    my( $reading, $characters ) = @_;
    my $this_pos = $characters->{$reading->position->minref};
    # This is a simple algorithm that treats every reading as different.
    # Eventually we will want to be able to specify how relationships
    # affect the character matrix.
    my $text = $reading->text;
    unless( exists $this_pos->{$text} ) {
	# We need to find what the next character is here, and record it.
	my @all_chr = sort { $a <=> $b } values( %$this_pos );
	if( @all_chr == 8 ) {
	    warn "Already have eight variants at position " 
		. $reading->position->minref . "; not adding " . $reading->text;
	    return '?';
	}
	$this_pos->{$text} = scalar @all_chr;
    }
    return $this_pos->{$text};
}

sub run_pars {
    my $self = shift;
    $self->make_character_matrix unless $self->has_character_matrix;

    # Set up a temporary directory for all the default Phylip files.
    my $phylip_dir = File::Temp->newdir();

    # We need an infile, and we need a command input file.
    $DB::single = 1;
    open( MATRIX, ">$phylip_dir/infile" ) or die "Could not write $phylip_dir/infile";
    my $rows = scalar @{$self->character_matrix};
    my $columns = scalar @{$self->character_matrix->[0]} - 1;
    print MATRIX "\t$rows\t$columns\n";
    foreach my $row ( @{$self->character_matrix} ) {
	my $wit = shift @$row;
	my $chars = join( '', @$row );
	print MATRIX sprintf( "%-10s%s\n", $wit, $chars );
    }
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
    if( -f "$phylip_dir/output" ) {
	open( OUTPUT, "$phylip_dir/output" ) or die "Could not open output for read";
	@error = <OUTPUT>;
	close OUTPUT;
    } else {
	push( @error, "Neither outtree nor output file was produced!" );
    }
    return( undef, join( '', @error ) );
}

no Moose;
__PACKAGE__->meta->make_immutable;
    
1;
