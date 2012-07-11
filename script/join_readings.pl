#!/usr/bin/env perl

use lib 'lib';
use feature 'say';
use strict;
use warnings;
use Getopt::Long;
use Lingua::Features::Structure;
use Text::Tradition::Directory;
use XML::Easy::Syntax qw/ $xml10_name_rx $xml10_namestartchar_rx /;
use TryCatch;

binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';
eval { no warnings; binmode $DB::OUT, ':utf8'; $DB::deep = 1000 };

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
	next unless $tinfo->{'name'} =~ /$lookfor/ || $tinfo->{'id'} eq $lookfor;
	my $tradition = $dir->lookup( $tinfo->{'id'} );
	my $c = $tradition->collation;

	# Anywhere in the graph that there is a reading that joins only to a single
	# successor, and neither of these have any relationships, just join the two
	# readings.
	my %gobbled;
	foreach my $rdg ( sort { $a->rank <=> $b->rank } $c->readings ) {
		next if $rdg->is_meta;
		next if $gobbled{$rdg->id};
		next if $rdg->grammar_invalid || $rdg->is_nonsense;
		next if $rdg->related_readings();
		my %seen;
		while( $c->sequence->successors( $rdg ) == 1 ) {
			my( $next ) = $c->reading( $c->sequence->successors( $rdg ) );
			die "Infinite loop" if $seen{$next->id};
			$seen{$next->id} = 1;
			last if $c->sequence->predecessors( $next ) > 1;
			last if $next->is_meta;
			last if $next->grammar_invalid || $next->is_nonsense;
			last if $next->related_readings();
			say "Joining readings $rdg and $next";
			$c->merge_readings( $rdg, $next, 1 );
		}
	}
	# Make sure we haven't screwed anything up
	foreach my $wit ( $tradition->witnesses ) {
		my $pathtext = $c->path_text( $wit->sigil );
		my $origtext = join( ' ', @{$wit->text} );
		die "Text differs for witness " . $wit->sigil 
			unless $pathtext eq $origtext;
		if( $wit->is_layered ) {
			$pathtext = $c->path_text( $wit->sigil.$c->ac_label );
			$origtext = join( ' ', @{$wit->layertext} );
			die "Ante-corr text differs for witness " . $wit->sigil
				unless $pathtext eq $origtext;
		}
	}

	$c->relations->rebuild_equivalence();
	$c->calculate_ranks();
	$dir->save( $tradition );
}