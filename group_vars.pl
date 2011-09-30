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

my $useful_vars = 0;
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
    
    # For all the groups with more than one member, make a group that contains
    # all contiguous vertices to connect them.
    # TODO Need to do pairwise comparison of groups - a variant location can
    # have both coincidental and genealogical variants!
    my %contig;
    my $conflict;
    foreach my $g ( @$groups ) {
        my @members = split( /,/, $g );
        next unless @members > 1;
        map { $contig{$_} = $g } @members;
        my $curr = pop @members;
        foreach my $m ( @members ) {
            foreach my $v ( $stemma->apsp->path_vertices( $curr, $m ) ) {
                $contig{$v} = $g unless exists $contig{$v};
                next if $contig{$v} eq $g;
                # print STDERR "Conflict at $v between group $g and group " 
                #     . $contig{$v} . "\n";
                $conflict = 1;
            }
        }
    }
    print join( " / ", @$groups ) . ' - ' . join( " / ", @$readings ) . ' - ';
    print $conflict ? "coincidental" : "genealogical";
    print "\n";
    $useful_vars++;
    
}
print "Found $useful_vars useful variants\n";

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
        push( @$groups, join( ',', @{$readings->{$var}} ) );
        push( @$text, $var );
    }
    return( $groups, $text );
}
