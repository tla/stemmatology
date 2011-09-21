package Text::Tradition::Parser::Tabular;

use strict;
use warnings;
use Text::CSV_XS;

=head1 NAME

Text::Tradition::Parser::Tabular

=head1 DESCRIPTION

Parser module for Text::Tradition to read an alignment table format, such as CSV.

=head1 METHODS

=over

=item B<parse>

parse( $graph, $graphml_string );

Takes an initialized Text::Tradition::Graph object and a string
containing the GraphML; creates the appropriate nodes and edges on the
graph.

=cut

sub parse {
    my( $tradition, $tab_str ) = @_;
    # TODO Allow setting of sep_char
    my $c = $tradition->collation; # shorthand
    my $csv = Text::CSV_XS->new( { binary => 1, # binary for UTF-8
        sep_char => "\t" } );
    my @lines = split( "\n", $tab_str );
    # Conveniently, we are basically receiving exactly the sort of alignment table
    # we might want to produce later.  May as well save it.
    my $alignment_table;
    foreach my $l ( @lines ) {
        my $status = $csv->parse( $l );
        if( $status ) {
            push( @$alignment_table, [ $csv->fields ] );
        } else {
            warn "Could not parse line $l: " . $csv->error_input;
        }
    }
    
    # Set up the witnesses we find in the first line
    my @witnesses;
    foreach my $sigil ( @{$alignment_table->[0]} ) {
        my $wit = $tradition->add_witness( 'sigil' => $sigil );
        $wit->path( [ $c->start ] );
        push( @witnesses, $wit );
    }
    
    # Now for the next rows, make nodes as necessary, assign their ranks, and 
    # add them to the witness paths.
    $DB::single = 1;
    foreach my $idx ( 1 .. $#{$alignment_table} ) {
        my $row = $alignment_table->[$idx];
        my $nodes = make_nodes( $c, $row, $idx );
        foreach my $w ( 0 .. $#{$row} ) {
            # push the appropriate node onto the appropriate witness path
            my $word = $row->[$w];
            if( $word ) {
                my $reading = $nodes->{$word};
                my $wit = $witnesses[$w];
                push( @{$wit->path}, $reading );
            } # else skip it for empty readings.
        }
    }
    
    
    # Collapse our lacunae into a single node and
    # push the end node onto all paths.
    $c->end->rank( scalar @$alignment_table );
    foreach my $wit ( @witnesses ) {
        my $p = $wit->path;
        my $last_rdg = shift @$p;
        my $new_p = [ $last_rdg ];
        foreach my $rdg ( @$p ) {
            if( $rdg->text eq '#LACUNA#' ) {
                # If we are in a lacuna already, drop this node.
                # Otherwise make a lacuna node and drop this node.
                unless( $last_rdg->is_lacuna ) {
                    my $l = $c->add_lacuna( $rdg->name );
                    $l->rank( $rdg->rank );
                    push( @$new_p, $l );
                    $last_rdg = $l;
                }
                $c->del_reading( $rdg );
            } else {
                # No lacuna, save the reading.
                push( @$new_p, $rdg );
            }
        }
        push( @$new_p, $c->end );
        $wit->path( $new_p );
    }
    
    # Join up the paths.
    $c->make_witness_paths;
    
    # Save the alignment table that was so handily provided to us.
    # TODO if we support other delimiters, we will have to re-export this
    # rather than saving the original string.
    $c->_save_csv( $tab_str );
}

sub make_nodes {
    my( $collation, $row, $index ) = @_;
    my %unique;
    foreach my $w ( @$row ) {
        $unique{$w} = 1 if $w;
    }
    my $ctr = 1;
    foreach my $w ( keys %unique ) {
        my $r = $collation->add_reading( "$index,$ctr" );
        $ctr++;
        $r->rank( $index );
        $r->text( $w );
        $unique{$w} = $r;
    }
    return \%unique;
}

1;