#!/usr/bin/env perl

use lib 'lib';
use strict;
use warnings;
use File::Basename;
use Getopt::Long;
use Text::Tradition;
use Text::Tradition::Directory;

binmode( STDOUT, ':utf8' );
binmode( STDERR, ':utf8' );

my( $name, $delete, $dbuser, $dbpass );
my( $list, $dsn ) = ( 1, 'dbi:SQLite:dbname=stemmaweb/db/traditions.db' );

GetOptions( 
	'r|rename=s' => \$name,
	'd|delete' => \$delete,
	'dsn=s' => \$dsn,
	'u|user=s' => \$dbuser,
	'p|pass=s' => \$dbpass,
	);
	
my @uuids = @ARGV;  # UUID is whatever is left over
my %dbargs = ( 'dsn' => $dsn );
$dbargs{'extra_args'} = { 'user' => $dbuser } if $dbuser;
$dbargs{'extra_args'}->{'password'} = $dbpass if $dbpass;
my $kdb = Text::Tradition::Directory->new( %dbargs );
$list = !$delete;

if( $delete ) {
	print STDERR "Must specify the UUID of a tradition to delete\n" unless @uuids;
	my $scope = $kdb->new_scope();
	foreach my $uuid ( @uuids ) {
		if( $kdb->exists( $uuid ) ) {
			$kdb->delete( $uuid );
		} else {
			print STDERR "No object found with ID $uuid\n";
		}
	}
}

if( $name ) {
	print STDERR "Must specify the UUID of a tradition to rename\n" unless @uuids;
	if( @uuids > 1 ) {
		print STDERR "Multiple traditions given for rename - do you really want to do that?\n";
	} else {
		my $scope = $kdb->new_scope();
		my $tradition = $kdb->lookup( $uuids[0] );
		if( $tradition ) {
			$tradition->name( $name );
			$kdb->save( $tradition );
		} else {
			print STDERR "Unable to find tradition @uuids to rename\n";
		}
	}
}

# Now list the DB contents if appropriate.
if( $list ) {
	my $scope = $kdb->new_scope();
	foreach my $tref ( $kdb->traditionlist ) {
		my $tid = $tref->{'id'};
		# If no IDs were given on the command line, list all traditions.
		if( @uuids ) {
			next unless grep { $_ eq $tid } @uuids;
		}
		my $t = $kdb->lookup( $tid );
		print STDERR "$tid: Tradition '" . $t->name . "'\n";
		my @wits = map { $_->sigil } $t->witnesses;
		print STDERR "...with witnesses @wits\n";
		my $c = $t->collation;
		print STDERR "...collation has " . scalar( $c->readings ) . " readings\n";
		print STDERR "...collation has " . scalar( $c->paths ) . " paths\n";
		print STDERR "...collation has " . scalar( $c->relationships ) . " relationship links\n";
		foreach my $s ( $t->stemmata ) {
			print STDERR "...associated stemma has graph " . $s->graph . "\n";
		}
	}
}
