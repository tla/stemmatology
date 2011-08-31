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
    my $table = $self->collation->make_alignment_table;
    # Push the names of the witnesses to initialize the rows of the matrix.
    my @matrix = map { [ $self->_normalize_ac( $_ ) ] } @{$table->[0]};
    $DB::single = 1;
    foreach my $token_index ( 1 .. $#{$table} ) {
        # First implementation: make dumb alignment table, caring about
        # nothing except which reading is in which position.
        my @chars = convert_characters( $table->[$token_index] );
        foreach my $idx ( 0 .. $#matrix ) {
            push( @{$matrix[$idx]}, $chars[$idx] );
        }
    }
    $self->_save_character_matrix( \@matrix );
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
    my %unique = ( '__UNDEF__' => 'X' );
    my $ctr = 0;
    foreach my $word ( @$row ) {
        if( $word && !exists $unique{$word} ) {
            $unique{$word} = chr( 65 + $ctr );
            $ctr++;
        }
    }
    if( scalar( keys %unique ) > 8 ) {
        warn "Have more than 8 variants on this location; pars will break";
    }
    my @chars = map { $_ ? $unique{$_} : $unique{'__UNDEF__' } } @$row;
    return @chars;
}

sub pars_input {
    my $self = shift;
    $self->make_character_matrix unless $self->has_character_matrix;
    my $matrix = '';
    my $rows = scalar @{$self->character_matrix};
    my $columns = scalar @{$self->character_matrix->[0]} - 1;
    $matrix .= "\t$rows\t$columns\n";
    foreach my $row ( @{$self->character_matrix} ) {
        $matrix .= join( '', @$row ) . "\n";
    }
    return $matrix;
}

sub run_pars {
    my $self = shift;

    # Set up a temporary directory for all the default Phylip files.
    my $phylip_dir = File::Temp->newdir();
    # We need an infile, and we need a command input file.
    open( MATRIX, ">$phylip_dir/infile" ) or die "Could not write $phylip_dir/infile";
    print MATRIX $self->pars_input();
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

no Moose;
__PACKAGE__->meta->make_immutable;
    
1;
