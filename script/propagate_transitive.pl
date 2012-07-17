#!/usr/bin/env perl

use lib 'lib';
use feature 'say';
use strict;
use warnings;
use Getopt::Long;
use Text::Tradition::Directory;
use TryCatch;

binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';
eval { no warnings; binmode $DB::OUT, ':utf8'; $DB::deep = 1000 };

my %TYPEVALUES = (
	orthographic => 1,
	spelling => 2,
	grammatical => 3,
	lexical => 3,
	collated => 50,
	);

my( $dbuser, $dbpass );
my $dsn = 'dbi:SQLite:dbname=stemmaweb/db/traditions.db';
my $testrun;

GetOptions( 
	'dsn=s'    => \$dsn,
	'u|user=s' => \$dbuser,
	'p|pass=s' => \$dbpass,
	'n|test'   => \$testrun,
	);

my $dbopts = { dsn => $dsn };
$dbopts->{extra_args}->{user} = $dbuser if $dbuser;
$dbopts->{extra_args}->{password} = $dbpass if $dbpass;

my $dir = Text::Tradition::Directory->new( $dbopts );

my $scope = $dir->new_scope();
my $lookfor = $ARGV[0] || '';
foreach my $tinfo ( $dir->traditionlist() ) {
	next if $tinfo->{'name'} eq 'xxxxx';
	next unless $tinfo->{'name'} =~ /$lookfor/ || $tinfo->{'id'} eq $lookfor;
	my $tradition = $dir->lookup( $tinfo->{'id'} );
	say "Looking at tradition " . $tradition->name;
	my $c = $tradition->collation;

	my $represented_by = {};
	my $representative = {};
	# For each set of ranked relationships, make all the implied links 
	# explicit. Start with orthographic readings
	push_rel_type( $c, 'orthographic', $representative, $represented_by );
	# then move on to spelling readings
	push_rel_type( $c, 'spelling', $representative, $represented_by );
	
	# Now all orth/spelling linked words are the same word for the purposes of
	# other colocated links, and in our representation hashes.
	# Go through the other relationships and propagate them to all words that are
	# the same word.
	foreach my $rel ( $c->relationships ) {
		my $relobj = $c->get_relationship( $rel );
		if( $relobj->type =~ /^(grammatical|lexical)$/ ) {
			my $r1pool = $represented_by->{$representative->{$rel->[0]}};
			my $r2pool = $represented_by->{$representative->{$rel->[1]}};
			# Error check
			if( check_distinct( $r1pool, $r2pool ) ) {
				map { propagate_rel( $c, $relobj->type, $_, @$r2pool ) } @$r1pool;
			} else {
				warn "Pools not distinct for " . join( ' and ', @$rel );
			}
		} elsif( $relobj->type eq 'transposition' ) {
			# We also need to propagate transposition links, but rather more strictly.
			propagate_rel( $c, 'transposition', map { $c->reading( $_ ) } @$rel );
		}
	}
	
	
	$dir->save( $tradition ) unless $testrun;
}

sub propagate_rel {
	my( $c, $type, @list ) = @_;
	my $curr = shift @list;
	while( @list ) {
		foreach my $r ( @list ) {
			next if $curr eq $r;
			# Check that the given relationship type exists between $curr and $r.
			# Also check that the given relationship type exists between $curr and
			# the same-type relationships of $r.
			my @candidates = ( $r );
			foreach my $rrel ( $r->related_readings() ) {
				next if $curr eq $rrel;
				my $rrelobj = $c->get_relationship( $r, $rrel );
				if( $rrelobj && $rrelobj->type eq $type ) {
					my $hasrrel = $c->get_relationship( $curr, $rrel );
					push( @candidates, $rrel ) unless $hasrrel;
				}
			}
			foreach my $cand ( @candidates ) {
				my $hasrel = $c->get_relationship( $curr, $cand );
				if( !$hasrel || $hasrel->type eq 'collated' ) {
					say STDERR "Propagating $type relationship $curr -> $cand";
					$c->add_relationship( $curr, $cand, { type => $type } );
				} elsif( $hasrel->type ne $type ) {
					warn "Found relationship conflict at $curr / $cand: "
						. $hasrel->type . " instead of $type"
						unless( $TYPEVALUES{$hasrel->type} < $TYPEVALUES{$type} );
				}
			}
		}
		$curr = shift @list;
	}
}

sub push_rel_type {
	my( $c, $type, $r2rep, $rep2r ) = @_;
	my %handled;
	foreach my $rdg ( $c->readings ) {
		next if $rdg->is_meta;
		next if $handled{"$rdg"};
		if( exists $r2rep->{"$rdg"} ) {
			$rdg = $r2rep->{"$rdg"};
		}
		# Get the specified relationships
		my @set = $rdg->related_readings( sub {
			$_[0]->colocated && ( $_[0]->type eq $type ||
			$TYPEVALUES{$_[0]->type} < $TYPEVALUES{$type} ) } );
		push( @set, $rdg );
		propagate_rel( $c, $type, @set ) if @set > 2;
		# Set up the representatives
		map { $r2rep->{"$_"} = $rdg } @set;
		$rep2r->{"$rdg"} = \@set;
		map { $handled{"$_"} = 1 } @set;
	}
}

sub check_distinct {
	my( $l1, $l2 ) = @_;
	my %seen;
	map { $seen{"$_"} = 1 } @$l1;
	map { return 0 if $seen{"$_"} } @$l2;
	return 1;
}
	
