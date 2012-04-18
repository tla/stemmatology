#!/usr/bin/env perl

use strict;
use warnings;
use lib 'lib';
use feature 'say';
use Test::More;
use Text::Tradition;
use Text::Tradition::Analysis qw/ run_analysis /;

binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';
eval { no warnings; binmode $DB::OUT, ':utf8'; $DB::deep = 1000 };

my $tradition = Text::Tradition->new(
	'input' => 'Self',
	'file' => 't/data/besoin.xml' );
$tradition->add_stemma( 'dotfile' => 't/data/besoin.dot' );

# Run the analysis of the tradition
my $results = run_analysis( $tradition );

my %expected = (
    3 => 1,
    28 => 1,
    39 => 1,
    73 => '',
    76 => 1,
    91 => '',
    93 => 1,
    94 => 1,
    99 => '',
    136 => '',
    142 => '',
    155 => 1,
    170 => 1,
    205 => 1,
    219 => 1,
    239 => 1,
    244 => 1,
    251 => 1,
    252 => 1,
    293 => 1,
    295 => 1,
    309 => 1,
    317 => '',
    318 => 1,
    319 => 1,
    328 => '',
    335 => 1,
    350 => '',
    361 => '',
    382 => '',
    385 => '',
    406 => 1,
    413 => 1,
    418 => '',
    493 => 1,
    497 => '',
    500 => '',
    515 => '',
    558 => '',
    632 => 1,
    634 => 1,
    636 => 1,
    685 => 1,
    737 => 1,
    742 => '',
    743 => '',
    744 => '',
    777 => '',
    780 => 1,
    837 => 1,
    897 => '',
    898 => '',
    925 => 1,
    952 => 1,
    954 => 1,
    969 => 1,
    972 => 1,
    973 => 1,
    1003 => 1,
    1004 => 1,
    1013 => 1,
);

# Look through the results
my $display = $ARGV[0];
my $c = $tradition->collation;
foreach my $row ( @{$results->{'variants'}} ) {
	if( $display ) {
		say sprintf( "=== Looking at rank %d (%s) ===", $row->{'id'},
			$row->{'genealogical'} ? 'genealogical' : 'not genealogical' );
		foreach my $rdg ( @{$row->{'readings'}} ) {
			my $parents = $rdg->{'reading_parents'};
			say sprintf( "Reading %s: %s", $rdg->{'readingid'}, 
				$rdg->{'conflict'} ? '(conflicted)' : '' );
			if( $parents && @$parents ) {
				say "\tParent reading(s) " . join( ', ', @$parents );
				foreach my $p ( @$parents ) {
					# Is there a relationship here?
					my $rel = $c->get_relationship( $rdg->{'readingid'}, $p );
					if( $rel ) {
						say sprintf( "\t* Relationship %s %s to parent %s",
							$rel->type, 
							$rel->annotation ? '('.$rel->annotation.')' : '', 
							$p );
					}
				}
			}
			say sprintf( "\t%d independent, %d followed, %d changed, %d unknown",
				$rdg->{'independent_occurrence'}, $rdg->{'followed'}, 
				$rdg->{'not_followed'}, $rdg->{'follow_unknown'} );
		}
	} else {
		# If not displaying, we're testing.
		# HACK to cope with formerly unuseful rows
		unless( exists $expected{$row->{'id'}} ) {
			$expected{$row->{'id'}} = 1;
		}
		my $gen_bool = $row->{'genealogical'} ? 1 : '';
		is( $gen_bool, $expected{$row->{'id'}}, 
			"Got expected genealogical result for rank " . $row->{'id'} );
		# If the row is genealogical, there should be one reading with no parents,
		# every reading should independently occur exactly once, and the total
		# number of changes + maybe-changes should equal the total number of
		# readings who have that one as a parent.
		if( $row->{'genealogical'} ) {
			# Make the mapping of parent -> child readings
			my %is_parent;
			my @has_no_parent;
			foreach my $rdg ( @{$row->{'readings'}} ) {
				my $parents = $rdg->{'reading_parents'} || [];
				foreach my $p ( @$parents ) {
					push( @{$is_parent{$p}}, $rdg->{'readingid'} );
				}
				push( @has_no_parent, $rdg->{'readingid'} ) unless @$parents;
			}
			# Test some stuff
			foreach my $rdg ( @{$row->{'readings'}} ) {
				is( $rdg->{'independent_occurrence'}, 1, 
					"Genealogical reading originates exactly once" );
			}
			is( @has_no_parent, 1, "Only one genealogical reading lacks a parent" );
		}
	}
}
done_testing() unless $display;
