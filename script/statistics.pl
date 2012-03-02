#!/usr/bin/env perl

use strict;
use warnings;
use lib 'lib';
use feature 'say';
use Text::Tradition::Directory;
use Text::Tradition::Analysis qw/ run_analysis /;

binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';
eval { no warnings; binmode $DB::OUT, ':utf8'; $DB::deep = 1000 };

my $args;
my $db = 'SQLite';
if( $ARGV[0] && $ARGV[0] eq 'mysql' ) {
	$db = shift @ARGV;
}
if( $db eq 'mysql' ) {
	$args = { 'dsn' => 'dbi:mysql:dbname=stemmaweb',
			  'extra_args' => { 'user' => 'stemmaweb', 'password' => 'l@chmann' } };
} else {
	$args = { 'dsn' => 'dbi:SQLite:dbname=stemmaweb/db/traditions.db' };
}
# the rest of @ARGV is tradition names

my $dir = Text::Tradition::Directory->new( $args );
my @traditions;
my @tlist = $dir->traditionlist;
if( @ARGV ) {
	# Get only the traditions named.
	foreach my $tid ( @tlist ) {
		push( @traditions, $tid->{'id'} )
			if grep { $tid->{'name'} =~ /\Q$_\E/ } @ARGV;
	}
} else {
	@traditions = map { $_->{'id'} } @tlist;
}

# Initialize our stats collection
my $omit = '(omitted)';

# Run the analysis of each tradition
# Look through the results
foreach my $tid ( @traditions ) {
	my $scope = $dir->new_scope();
	my $tradition = $dir->lookup( $tid );
	printf( "\n**** TRADITION %s ****\n", $tradition->name );
	my $c = $tradition->collation;
	my $results = run_analysis( $tradition );
	my %stats;
	my %rels_found;
	foreach my $row ( @{$results->{'variants'}} ) {
		# say sprintf( "=== Looking at rank %d (%s) ===", $row->{'id'},
		# 	$row->{'genealogical'} ? 'genealogical' : 'not genealogical' );
		my $rdgdir = {};
		map { $rdgdir->{$_->{'readingid'}} = $_ } @{$row->{'readings'}};
		# Look for reading parents and the relationships between them.
		my %seen_rel;
		foreach my $rdg ( keys %$rdgdir ) {
			my $rhash = $rdgdir->{$rdg};
			my $parents = $rhash->{'reading_parents'};
			if( $parents && @$parents ) {
				say sprintf( " - reading %s ( %s ) has %d possible origins",
					$rdg, $rhash->{'text'}, scalar @$parents )
					unless @$parents == 1;
				foreach my $p ( @$parents ) {
					# Is there a relationship here?
					my $rel = $c->get_relationship( $rdg, $p );
					my $type;
					if( $rel ) {
						# Yes there is - get its type and figure stuff out.
						$type = $rel->type;
					} elsif( $rdg eq $omit ) {
						$type = 'deletion';
					} elsif( $p eq $omit ) {
						$type = 'addition';
					} # TODO need to manage transposition
					if( $type ) {
						# Note that there was an instability of this type
						$seen_rel{$type} = 1;
						$stats{$type} = {} unless exists $stats{$type};
						# Calculate, in this spot, how often the form shifted
						# vs. how often it stayed the same.
						# Add the number of times this form appeared
						add_to_hash( $stats{$type}, 'shifts', 
									 $rhash->{'independent_occurrence'} );
						# Add the number of times this form was followed
						add_to_hash( $stats{$type}, 'follows', $rhash->{'followed'} );
						# TODO work out whether not_followed gets included after iteration
					}
				} # foreach parent
				
			} # if parents
			foreach my $k ( keys %seen_rel ) {
				add_to_hash( \%rels_found, $k, 1 );
			}
		} # foreach rdg
	}
	# Print out the stats
	# First see how stable the text was
	my $total = $c->end->rank - 1;
	say sprintf( "Total locations %d, total variant locations %d", 
				 $total, $results->{'variant_count'} );
	say $results->{'genealogical_count'} 
		. " variant locations entirely followed the stemma";
	say $results->{'conflict_count'}
		. " variant readings conflicted with the stemma";
	foreach my $k ( keys %rels_found ) {
		my $shifts = $rels_found{$k};
		say "Had $shifts total $k-type shifts in $total locations";
	}
	foreach my $k ( keys %stats ) {
		say "\tType $k:";
		say sprintf( "\tUnstable readings shifted %d times, and were followed %d times",
					 $stats{$k}->{'shifts'}, $stats{$k}->{'follows'} );
	}
}

sub add_to_hash {
	my( $hash, $key, $num ) = @_;
	unless( exists $hash->{$key} ) {
		$hash->{$key} = 0;
	}
	$hash->{$key} += $num;
}