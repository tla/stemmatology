#!/usr/bin/env perl

use lib 'lib';
use feature 'say';
use strict;
use warnings;
use JSON;
use Text::Tradition::Directory;

binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';
eval { no warnings; binmode $DB::OUT, ':utf8'; $DB::deep = 1000 };

my %VARS = (
	DBTYPE => 'mysql',
	DBHOST => '127.0.0.1',
	DBPORT => '3006',
	DBNAME => 'idpresult',
	DSN => undef,
	DBUSER => undef,
	DBPASS => undef,
	TMPDIR => '/var/tmp'
);

if( -f "/etc/graphcalc.conf" ) {
	# Read the variables in from here.
	open( GCCONF, "/etc/graphcalc.conf" ) 
		or die "Could not open configuration file /etc/graphcalc.conf";
	while(<GCCONF>) {
		chomp;
		s/^\s+//;
		my( $name, $val ) = split( /\s*\=\s*/, $_ );
		if( exists $VARS{$name} ) {
			$VARS{$name} = $val;
		}
	}
	close GCCONF;
}
unless( $VARS{DSN} ) {
	$VARS{DSN} = sprintf( "dbi:%s:dbname=%s;host=%s;port=%s",
		$VARS{DBTYPE}, $VARS{DBNAME}, $VARS{DBHOST}, $VARS{DBPORT} );
}

my $dirargs = {};
$dirargs->{user} = $VARS{DBUSER} if $VARS{DBUSER};
$dirargs->{password} = $VARS{DBPASS} if $VARS{DBPASS};
my $dir = Text::Tradition::Directory->new( 
	'dsn' => $VARS{DSN}, 'extra_args' => $dirargs );

my $scope = $dir->new_scope();
my $dumpfile = $VARS{TMPDIR}.'/idpbackup.json';
open( IDPBACKUP, ">$dumpfile" )
	or die "Could not open dump file $dumpfile for writing";
binmode IDPBACKUP, ':utf8';
$dir->scan( sub {
	say IDPBACKUP JSON->new->allow_blessed->convert_blessed->encode( @_ );
});
close IDPBACKUP;
