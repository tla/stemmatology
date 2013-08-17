#!/usr/bin/env perl

use lib 'lib';
use feature 'say';
use strict;
use warnings;
use IPC::Run qw/ run /;
use JSON;
use Text::Tradition::Analysis::Result;
use Text::Tradition::Directory;
use TryCatch;

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

say 'Dropping tables in ' . $VARS{DBNAME};
my @connectargs = (	'-h', $VARS{DBHOST}, '-P', $VARS{DBPORT}, '-u'.$VARS{DBUSER}, 
	'-p'.$VARS{DBPASS}, $VARS{DBNAME} );
my( $ret, $err );
my @dump = ( 'mysqldump', '--add-drop-table', '--no-data', @connectargs );
my @grep = ( 'grep', '^DROP' );
my @sort = ( 'sort', '-r' );
my @drop = ( 'mysql', @connectargs );
run( \@dump, '|', \@grep, '|', \@sort, '|', \@drop, '>' ,\$ret, '2>', \$err )
	or die "Drop command returned $?:\n$err";

my $dirargs = { create => 1 };
$dirargs->{user} = $VARS{DBUSER} if $VARS{DBUSER};
$dirargs->{password} = $VARS{DBPASS} if $VARS{DBPASS};
my $dir = Text::Tradition::Directory->new( 
	'dsn' => $VARS{DSN}, 'extra_args' => $dirargs );

my $scope = $dir->new_scope();
my $dumpfile = $VARS{TMPDIR}.'/idpbackup.json';
open( IDPBACKUP, "$dumpfile" )
	or die "Could not open dump file $dumpfile for reading";
binmode IDPBACKUP, ':utf8';
my $nodel;
my $ctr = 0;
while( <IDPBACKUP> ) {
	chomp;
	$ctr++;
	say STDERR "...$ctr results" unless ( $ctr % 500 );
	my $struct = from_json( $_ );
	my $result = Text::Tradition::Analysis::Result->new( $struct );
	if( $result ) {
		try {
			$dir->store( $result->object_key => $result );
		} catch ($err) {
			$nodel = 1;
			if( $err =~ /already in use / || $err =~ /Duplicate/) {
				say STDERR "Duplicate entry " . $result->object_key;
			} else {
				say STDERR "Error saving result " . $result->object_key . ": $err";
			}
		}
	} else {
		warn "Failed to parse result in $_";
		$nodel = 1;
	}
}
close IDPBACKUP;
say "Done.";

unlink( $dumpfile ) unless $nodel;