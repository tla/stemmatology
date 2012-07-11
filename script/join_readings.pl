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
	
	# Save/update the current path texts
	foreach my $wit ( $tradition->witnesses ) {
		my @pathtext = split( /\s+/, $c->path_text( $wit->sigil ) );
		$wit->text( \@pathtext );
		if( $wit->is_layered ) {
			my @layertext = split( /\s+/, $c->path_text( $wit->sigil.$c->ac_label ) );
			$wit->layertext( \@layertext );
		}
	}
	
	# Do the deed
	$c->compress_readings();
	# ...and save it.
	$dir->save( $tradition );
}