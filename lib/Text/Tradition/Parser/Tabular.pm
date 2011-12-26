package Text::Tradition::Parser::Tabular;

use strict;
use warnings;
use Text::CSV_XS;

=head1 NAME

Text::Tradition::Parser::Tabular

=head1 SYNOPSIS

  use Text::Tradition;
  
  my $t_from_file = Text::Tradition->new( 
    'name' => 'my text',
    'input' => 'Tabular',
    'file' => '/path/to/collation.csv',
    'sep_char' => ','
    );
    
  my $t_from_string = Text::Tradition->new( 
    'name' => 'my text',
    'input' => 'Tabular',
    'string' => $tab_separated_collation,
    'sep_char' => "\t",
    );

=head1 DESCRIPTION

Parser module for Text::Tradition to read an alignment table format, such as CSV.

=head1 METHODS

=head2 B<parse>( $tradition, $option_hash )

Takes an initialized tradition and a set of options; creates the
appropriate nodes and edges on the graph, as well as the appropriate
witness objects.  The $option_hash must contain either a 'file' or a
'string' argument with the table to be parsed; it may also contain a 
'sep_char' argument to specify how the fields are separated.

The table should have witnesses arranged in columns, with the witness sigla
in the first row.  Empty cells are interpreted as omissions (and thus
stemmatologically relevant.) Longer lacunae in the text, to be disregarded
in cladistic analysis, may be represented by filling the appropriate cells
with the tag '#LACUNA#'.

If a witness name ends in the collation's ac_label, it will be treated as
an 'ante-correction' version of the 'main' witness whose sigil it shares.

=begin testing

use Text::Tradition;
binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";
eval { no warnings; binmode $DB::OUT, ":utf8"; };

my $csv = 't/data/florilegium.csv';
my $t = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'Tabular',
    'file'  => $csv,
    'sep_char' => ',',
    );

is( ref( $t ), 'Text::Tradition', "Parsed florilegium CSV file" );

### TODO Check these figures
if( $t ) {
    is( scalar $t->collation->readings, 313, "Collation has all readings" );
    is( scalar $t->collation->paths, 2877, "Collation has all paths" );
    is( scalar $t->witnesses, 13, "Collation has all witnesses" );
}

=end testing

=cut

sub parse {
    my( $tradition, $opts ) = @_;
    my $c = $tradition->collation; # shorthand
    my $csv = Text::CSV_XS->new( { 
        binary => 1, # binary for UTF-8
        sep_char => exists $opts->{'sep_char'} ? $opts->{'sep_char'} : "\t" } 
        );
    
    my $alignment_table;
    if( exists $opts->{'string' } ) {
        my @lines = split( "\n", $opts->{'string'} );
        foreach my $l ( @lines ) {
            my $status = $csv->parse( $l );
            if( $status ) {
                push( @$alignment_table, [ $csv->fields ] );
            } else {
                warn "Could not parse line $l: " . $csv->error_input;
            }
        }
    } elsif( exists $opts->{'file'} ) {
        open( my $fh, $opts->{'file'} ) 
            or warn "Could not open input file " . $opts->{'file'};
        binmode( $fh, ':utf8' );
        while( my $row = $csv->getline( $fh ) ) {
            push( @$alignment_table, $row );
        }
        close $fh;
    } else {
        warn "Could not find string or file option to parse";
        return;
    }

    # Set up the witnesses we find in the first line
    my @witnesses;
    my %ac_wits;  # Track these for later removal
    foreach my $sigil ( @{$alignment_table->[0]} ) {
        my $wit = $tradition->add_witness( 'sigil' => $sigil );
        $wit->path( [ $c->start ] );
        push( @witnesses, $wit );
        my $aclabel = $c->ac_label;
        if( $sigil =~ /^(.*)\Q$aclabel\E$/ ) {
            $ac_wits{$1} = $wit;
        }
    }
    
    # Now for the next rows, make nodes as necessary, assign their ranks, and 
    # add them to the witness paths.
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
                    my $l = $c->add_reading( {
                		'collation' => $c,
                		'id' => $rdg->name,
                		'is_lacuna' => 1,
                		} );
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
    
    # Fold any a.c. witnesses into their main witness objects, and
    # delete the independent a.c. versions.
    foreach my $a ( keys %ac_wits ) {
        my $main_wit = $tradition->witness( $a );
        next unless $main_wit;
        my $ac_wit = $ac_wits{$a};
        $main_wit->uncorrected_path( $ac_wit->path );
        $tradition->del_witness( $ac_wit );
    }

    # Join up the paths.
    $c->make_witness_paths;
}

sub make_nodes {
    my( $collation, $row, $index ) = @_;
    my %unique;
    foreach my $w ( @$row ) {
        $unique{$w} = 1 if $w;
    }
    my $ctr = 1;
    foreach my $w ( keys %unique ) {
    	my $rargs = {
    		'collation' => $collation,
    		'id' => "$index,$ctr",
    		'rank' => $index,
    		'text' => $w,
    		};
        my $r = $collation->add_reading( $rargs );
        $unique{$w} = $r;
        $ctr++;
    }
    return \%unique;
}

1;

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews E<lt>aurum@cpan.orgE<gt>
