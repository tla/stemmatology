#!/usr/bin/env perl

use lib 'lib';
use strict;
use warnings;
use Text::Tradition;
use Text::Tradition::Stemma;

binmode STDERR, ":utf8";
binmode STDOUT, ":utf8";
eval { no warnings; binmode $DB::OUT, ":utf8"; };

# Parse the tradition data
my $informat = 'Self';

my %args = ( 'input' => $informat,
             'file'  => $ARGV[0] );
my $tradition = Text::Tradition->new( %args );

# Parse the stemma hypothesis
my $stemma = Text::Tradition::Stemma->new( 
    'collation' => $tradition->collation,
    'dot' => $ARGV[1],
    );

# We have the collation, so get the alignment table with witnesses in rows.
# Also return the reading objects in the table, rather than just the words.

my $all_wits_table = $tradition->collation->make_alignment_table( 'refs' );

# For each column in the alignment table, we want to see if the existing
# groupings of witnesses match our stemma hypothesis. We also want, at the
# end, to produce an HTML table with all the variants.
my $html_columns = 0;
my $html_data = [];
my $total = 0; # Keep track of the total number of variant locations

# Strip the list of sigla and save it for correlation to the readings.
my $col_wits = shift @$all_wits_table;
    
foreach my $i ( 0 .. $#$all_wits_table ) {
    # For each column in the table, group the readings by witness.
    my $rdg_wits = {};
    my $col_rdgs = shift @$all_wits_table;
    my $rank;
    foreach my $j ( 0 .. $#{$col_rdgs} ) {
        my $rdg = $col_rdgs->[$j];
        $rank = $rdg->rank if $rdg;  # Save the rank for later display
        my $rdg_text = '(omitted)';  # Initialize in case of empty reading
        if( $rdg ) {
            $rdg_text = $rdg->is_lacuna ? undef : $rdg->text; # Don't count lacunae
        }
        if( defined $rdg_text ) {
            # Initialize the witness array if we haven't got one yet
            $rdg_wits->{$rdg_text} = [] unless $rdg_wits->{$rdg_text};
            # Add the relevant witness, subject to a.c. logic
            add_variant_wit( $rdg_wits->{$rdg_text}, $col_wits->[$j] );
        }
    }
    
    # See if this column has any potentially genealogical variants.
    # If not, skip to the next.
    $total++ unless scalar keys %$rdg_wits == 1;
    my( $groups, $readings ) = useful_variant( $rdg_wits );
    next unless $groups && $readings;  
    $html_columns = scalar @$groups if scalar @$groups > $html_columns;
    
    # We can already look up witnesses for a reading; we also want to look
    # up readings for a given witness.
    my $group_readings = {};
    foreach my $x ( 0 .. $#$groups ) {
        $group_readings->{wit_stringify( $groups->[$x] )} = $readings->[$x];
    }
    
    # For all the groups with more than one member, collect the list of all
    # contiguous vertices needed to connect them.
    # TODO: deal with a.c. reading logic
    my $sc = analyze_variant_location( $group_readings, $groups, $stemma->apsp );
    print wit_stringify( $groups ) . ' - ' . join( " / ", @$readings ) . "\n";
    foreach my $rdg ( keys %$sc ) {
        my $var = $sc->{$rdg};
        print "\tReadings '$rdg' and '$var' are not genealogical\n";
    }
    
    # Now run the same analysis given the calculated distance tree(s).
    foreach my $tree ( 0 .. $#{$stemma->distance_trees} ) {
        my $dc = analyze_variant_location( $group_readings, $groups,    
                                           $stemma->distance_apsps->[$tree] );
        foreach my $rdg ( keys %$dc ) {
            my $var = $dc->{$rdg};
            print "\tReadings '$rdg' and '$var' disregarded by parsimony on tree $tree\n";
        }
    }

    # Record that we used this variant in an analysis
    push( @$html_data, [ $rank, $readings, $sc ] );
}

# Save the stemma picture
open( STEMMA, ">stemma_graph.svg" ) or die "Could not open stemma graph to write";
binmode STEMMA, ":utf8";
print STEMMA $stemma->as_svg;
close STEMMA;

printf( "Ran analysis on %d / %d variant locations\n", scalar @$html_data, $total );

sub analyze_variant_location {
    my( $group_readings, $groups, $apsp ) = @_;
    my %contig;
    my $conflict = {};
    foreach my $g ( sort { scalar @$b <=> scalar @$a } @$groups ) {
        my @members = @$g;
        my $gst = wit_stringify( $g );
        map { $contig{$_} = $gst } @members; # The witnesses need themselves to be 
                                             # in their collection.
        next unless @members > 1;
        my $curr = pop @members;
        foreach my $m ( @members ) {
            foreach my $v ( $apsp->path_vertices( $curr, $m ) ) {
                $contig{$v} = $gst unless exists $contig{$v};
                next if $contig{$v} eq $gst;
                # print STDERR "Conflict at $v between group $gst and group " 
                #     . $contig{$v} . "\n";
                # Record what is conflicting.
                $conflict->{$group_readings->{$gst}} = $group_readings->{$contig{$v}};
            }
        }
    }
    return $conflict;
}

# Add the variant, subject to a.c. representation logic.
# This assumes that we will see the 'main' version before the a.c. version.
sub add_variant_wit {
    my( $arr, $wit ) = @_;
    my $acstr = $tradition->collation->ac_label;
    my $skip;
    if( $wit =~ /^(.*)\Q$acstr\E$/ ) {
        my $real = $1;
        $skip = grep { $_ =~ /^\Q$real\E$/ } @$arr;
    } 
    push( @$arr, $wit ) unless $skip;
}

# Return an answer if the variant is useful, i.e. if there are at least 2 variants
# with at least 2 witnesses each.
sub useful_variant {
    my( $readings ) = @_;
    my $total = keys %$readings;
    foreach my $var ( keys %$readings ) {
        $total-- if @{$readings->{$var}} == 1;
    }
    return( undef, undef ) if $total <= 1;
    my( $groups, $text );
    foreach my $var ( keys %$readings ) {
        push( @$groups, $readings->{$var} );
        push( @$text, $var );
    }
    return( $groups, $text );
}

# Take an array of witness groupings and produce a string like
# A,B / C,D,E / F

sub wit_stringify {
    my $groups = shift;
    my @gst;
    # If we were passed an array of witnesses instead of an array of 
    # groupings, then "group" the witnesses first.
    unless( ref( $groups->[0] ) ) {
        my $mkgrp = [ $groups ];
        $groups = $mkgrp;
    }
    foreach my $g ( @$groups ) {
        push( @gst, join( ',', @$g ) );
    }
    return join( ' / ', @gst );
}
    