#!/usr/bin/env perl

use lib 'lib';
use strict;
use warnings;
use Text::Tradition;
use Text::Tradition::Stemma;

binmode STDERR, ":utf8";
binmode STDOUT, ":utf8";
eval { no warnings; binmode $DB::OUT, ":utf8"; };

my $informat = 'TEI';
my $inbase;
my $linear = 1;

# Parse the tradition data

my $input = $ARGV[0];
my @lines;
open( INFILE, "$input" ) or die "Could not read $input";
binmode INFILE, ':utf8';
@lines = <INFILE>;
close INFILE;
$input = join( '', @lines );

my %args = ( $informat => $input,
             'linear' => $linear );
$args{'base'} = $inbase if $inbase;
my $tradition = Text::Tradition->new( %args );

# Parse the stemma hypothesis
my $stemma = Text::Tradition::Stemma->new( 
    'collation' => $tradition->collation,
    'dot' => $ARGV[1],
    );

# We have the collation, so get the alignment table with witnesses in rows.

my $all_wits_table = $tradition->collation->make_alignment_table( 1 );

# For each column in the alignment table, we want to see if the existing
# groupings of witnesses match our stemma hypothesis.  First let's just go 
# through the groupings.

# Strip the list of sigla and save it for correlation to the readings.
my $col_wits = shift @$all_wits_table;
    
# For each column in the table, group the readings by witness.

my $used_vars = 0;
foreach my $i ( 0 .. $#$all_wits_table ) {
    my $rdg_wits = {};
    my $col_rdgs = shift @$all_wits_table;
    foreach my $j ( 0 .. $#{$col_rdgs} ) {
        my $rdg = $col_rdgs->[$j];
        $rdg = '' unless $rdg;   # We care about empty readings
        $rdg = undef if $rdg eq '#LACUNA#'; # ... unless they're lacunas
        if( $rdg ) {
            $rdg_wits->{$rdg} = [] unless $rdg_wits->{$rdg};
            add_variant_wit( $rdg_wits->{$rdg}, $col_wits->[$j] );
        }
    }
    
    my( $groups, $readings ) = useful_variant( $rdg_wits );
    next unless $groups && $readings;        
    
    # We can look up witnesses for a reading; we also want to look up readings
    # for a given witness.
    my $group_readings = {};
    foreach my $x ( 0 .. $#$groups ) {
        $group_readings->{wit_stringify( $groups->[$x] )} = $readings->[$x];
    }
    
    # For all the groups with more than one member, collect the list of all
    # contiguous vertices needed to connect them.
    # TODO: deal with a.c. reading logic
    my $conflict = analyze_variant_location( $group_readings, $groups, $stemma->apsp );
    print wit_stringify( $groups ) . ' - ' . join( " / ", @$readings ) . "\n";
    foreach my $rdg ( keys %$conflict ) {
        my $var = $conflict->{$rdg};
        print "\tReadings '$rdg' and '$var' are not genealogical\n";
    }
    
    # Now run the same analysis given a distance tree.
    my $distance_apsp = $stemma->distance_trees->[0]->APSP_Floyd_Warshall();
    $conflict = analyze_variant_location( $group_readings, $groups, $distance_apsp );
    foreach my $rdg ( keys %$conflict ) {
        my $var = $conflict->{$rdg};
        print "\tReadings '$rdg' and '$var' disregarded by parsimony\n";
    }

    # Record that we used this variant in an analysis
    $used_vars++;
    
}
print "Found $used_vars useful variants in this analysis\n";
# Save the stemma picture
open( STEMMA, ">stemma_graph.svg" ) or die "Could not open stemma graph to write";
binmode STEMMA, ":utf8";
print STEMMA $stemma->as_svg;
close STEMMA;

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
    