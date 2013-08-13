#!/usr/bin/env perl

use strict;
use warnings;
use feature 'say';
use lib '/home/tla/cpanmods/Text-SenseClusters-1.03/lib';
use Text::SenseClusters::Simat;
use Text::Tradition;
use Text::WagnerFischer qw/distance/;

binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';

# Get our arguments
my( $traditionfile, $threshold ) = @ARGV;
$threshold = 0.99 unless $threshold;

# Load up a tradition
my $t;
my $m;

$t = Text::Tradition->new( 
		file => $traditionfile,
		input => 'Self' );
say STDERR "Parsed tradition file";
my $c = $t->collation;

# Get the cosine similarity values
my ( $matrix ) = make_matrix( $t );

# For each relationship in the graph, see how it compares to other node pairs
# rated > $threshold

foreach my $pair ( $c->relationships ) {
	my $rel = $c->get_relationship( $pair );
	my( $rx, $ry ) = map { $c->reading( $_ ) } sort @$pair;

	say STDERR "Checking relationship $rx -- $ry, of type " . $rel->type;
	my $matches = 0;
	my @matched_rels;
	foreach my $val ( sort { $a<=>$b } keys %{$matrix->{"$rx"}->{"$ry"}} ) {
		$matches++;
		foreach my $mpair ( @{$matrix->{"$rx"}->{"$ry"}->{"$val"}} ) {
			my $mrel = $c->get_relationship( $mpair );
			my $mreltype = $mrel ? $mrel->type : '(no relation)';
			my( $mx, $my ) = map { $c->reading( $_ ) } sort @$mpair;
			push( @matched_rels, sprintf( "%f: %s (%s) -- %s (%s), type %s",
				$val, $mx, $mx->text, $my, $my->text, $mreltype ) );
		}
	}
	say sprintf( "Matches for %s (%s) -- %s (%s), type %s",
		$rx, $rx->text, $ry, $ry->text, $rel->type );
	foreach ( @matched_rels ) {
		say "\t$_";
	}	
}
	

sub make_matrix {
	my @comm = $c->calculate_common_readings();
	my %nextcomm;
	my $ri = 0;
	foreach my $cr ( sort { $a->rank <=> $b->rank } @comm ) {
		until( $cr->rank == $ri ) {
			$nextcomm{$ri++} = $cr->rank;
		}
	}
	until ( $ri == $c->end->rank ) {
		$nextcomm{$ri++} = $c->end->rank;
	}

	# Find all the relatable pairs
	my $grid = {};
	my %analyzed;
	my $rct = 0;
	foreach my $rx ( $c->readings ) {
		next if $rx->is_meta();
		# Have to compare each reading with each other, so do this only once
		$analyzed{"$rx"} = 1;
		$rct++;
		say STDERR "Looking at reading $rct ( $rx )";
		foreach my $ry ( $c->readings ) {
			next if $ry->is_meta();
			next if $analyzed{"$ry"};
		
			# Get their textual and graph distances from each other.
			my $vector = { 
				textdiff => distance( $rx->text, $ry->text ),
				rankdiff => abs( $rx->rank - $ry->rank )
			};
			$grid->{"$rx"}->{"$ry"} = $vector;
		
			# Do they share one or more witnesses?
			my %rxwits;
			my $sharewit = 0;
			map { $rxwits{$_} = 1 } $rx->witnesses;
			foreach my $rywit ( $ry->witnesses ) {
				if( $rxwits{$rywit} ) {
					$sharewit++;
					$vector->{'reachable'} = 1;
					last;
				}
			}
			$vector->{'share_witness'} = $sharewit;
			# Enough with the analysis if they do share witnesses.
			next if $sharewit;
		
			# Is one node reachable from the other, even if they don't share a
			# witness?
			$vector->{'reachable'} = $nextcomm{$rx->rank} == $nextcomm{$ry->rank} ? 0 : 1;
		}
	}

	# Construct the vector list
	# Dimensions are textdiff, rankdiff, reachable, share_witness

	my( $i, $values ) = ( 0, 0 );
	my $vecindex = {};
	my @keys;
	my @lines;
	foreach my $rx ( keys %$grid ) {
		foreach my $ry ( keys %{$grid->{$rx}} ) {
			my $vec = $grid->{$rx}->{$ry};
			next if $vec->{'reachable'}; # Skip all but colocations
			$vecindex->{++$i} = [ $rx, $ry ];
			# Note the number of keys from the first vector we look at.
			unless( @keys ) {
				@keys = sort keys( %$vec );
			}
			# Construct the matrix.
			my @fields;
			foreach my $j ( 0 .. $#keys ) {
				my $v = $vec->{$keys[$j]};
				if( $v ) {
					push( @fields, $j+1, $v );
					$values++;
				}
			}
			push( @lines, join( " ", @fields ) );
		}
	}

	my $matrix .= "$i " . scalar( @keys ) . " $values\n";
	foreach( @lines ) {
		$matrix .= "$_\n";
	}

	# Open a filehandle on the matrix string we get.
	open( my $matrix_fh, '<', \$matrix ) or die "Could not open filehandle on string";
	
	# Now send the remainder of the filehandle for calculation.
	my $simmatrix = Text::SenseClusters::Simat::simat( $matrix_fh );

	# Match the returned values with the actual node pair comparisons they refer to.
	my $cosine_values = {};
	foreach my $x ( keys %$simmatrix ) {
		foreach my $y ( keys %{$simmatrix->{$x}} ) {
			next unless $x < $y; # skip self-comparison & duplicates
			my $val = $simmatrix->{$x}->{$y};
			# If the value is below our threshold of interest, skip it.
			next if $val < $threshold;
			# Sort the lookup of a pair alphabetically by reading ID.
			my ( $r1x, $r1y ) = sort @{$vecindex->{$x}};
			my ( $r2x, $r2y ) = sort @{$vecindex->{$y}};
			push( @{$cosine_values->{$r1x}->{$r1y}->{$val}}, [ $r2x, $r2y ] );
			push( @{$cosine_values->{$r2x}->{$r2y}->{$val}}, [ $r1x, $r1y ] );
		}
	}
	
	return $cosine_values;
}
