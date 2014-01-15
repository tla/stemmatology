package Text::Tradition::Analysis::IDPUtil;

use strict;
use warnings;
use feature 'say';
use Exporter 'import';
use vars qw/ @EXPORT_OK /;
use Data::Validate::IP qw/ is_ipv4 is_ipv6 /;
use IPC::Run qw/ run /;

@EXPORT_OK = qw/ read_config connect_db connect_db_create reset_db /;

=head1 NAME

Text::Tradition::IDPUtil - common utilities for talking to the IDP solver
and results database

=head1 DESCRIPTION

This package contains a set of utilities for handling IDP calculations on
the stemma graph properties, and their storage in a database.

=head1 SUBROUTINES

=head2 read_config

Read the machine configuration file to find out how to talk to our database
and our Gearman instance. Returns a hash of untainted variables.

=cut

sub read_config {
	### Configurable variables
	my %opts = (
		DBTYPE => 'mysql',
		DBHOST => '127.0.0.1',
		DBPORT => '3306',
		DBNAME => 'idpresult',
		DSN => undef,
		DBUSER => undef,
		DBPASS => undef,
		GEARMAN_SERVER => '127.0.0.1:4730',
	);

	if( -f "/etc/graphcalc.conf" ) {
		# Read the variables in from here.
		open( GCCONF, "/etc/graphcalc.conf" ) 
			or die "Could not open configuration file /etc/graphcalc.conf";
		while(<GCCONF>) {
			chomp;
			s/^\s+//;
			my( $name, $val ) = split( /\s*\=\s*/, $_ );
			if( $name eq 'GEARMAN_SERVER' ) {
				# Minimally validate and untaint the value.
				my( $gsip, $gsport ) = split( /:/, $val );
				my $ipv = Data::Validate::IP->new();
				my $ugsip = $ipv->is_ipv4( $gsip );
				unless( $ugsip ) {
					$ugsip = $ipv->is_ipv6( $gsip );
				}
				if( $ugsip && $gsport =~ /^(\d+)$/ ) {
					$opts{$name} = "$ugsip:$1";
				}
			} elsif( exists $opts{$name} ) {
				$opts{$name} = $val;
			}
		}
		close GCCONF;
	}
	unless( $opts{DSN} ) {
		$opts{DSN} = sprintf( "dbi:%s:dbname=%s;host=%s;port=%s",
			$opts{DBTYPE}, $opts{DBNAME}, $opts{DBHOST}, $opts{DBPORT} );
	}

	return %opts;
}

=head2 connect_db( %config )
=head2 connect_db_create( %config )

Given a configuration has as obtained from read_config, return a connection to
the appropriate KiokuDB store. If the _create variant is called, the DB tables
are created if they don't yet exist.

=cut

sub connect_db {
	my $db = Text::Tradition::Directory->new( _get_db_connect_opts( @_ ) );
	return $db;
}

sub connect_db_create {
	my %connopts = _get_db_connect_opts( @_ );
	$connopts{extra_args}->{create} = 1;
	my $db = Text::Tradition::Directory->new( %connopts );
	return $db;
}

sub _get_db_connect_opts {
	my %opts = @_;
	my %dbconnopts = ( dsn => $opts{DSN} );
	if( exists $opts{DBUSER} ) {
		$dbconnopts{extra_args}->{user} = $opts{DBUSER};
	}
	if( exists $opts{DBPASS} ) {
		$dbconnopts{extra_args}->{password} = $opts{DBPASS};
	}
	return %dbconnopts;
}

=head2 $status = reset_db( %config ) {

Attempts to wipe the relevant database. Currently this can be done for SQLite 
and MySQL. If the returned $status is not "OK", something went wrong.

=cut

sub reset_db {
	my %opts = @_;
	my $status = "OK";
	if( $opts{DBTYPE} eq 'mysql' ) {
		say 'Dropping tables in ' . $opts{DBNAME};
		my @connectargs = (	'-h', $opts{DBHOST}, '-P', $opts{DBPORT}, '-u'.$opts{DBUSER}, 
			'-p'.$opts{DBPASS}, $opts{DBNAME} );
		my( $ret, $err );
		my @dump = ( 'mysqldump', '--add-drop-table', '--no-data', @connectargs );
		my @grep = ( 'grep', '^DROP' );
		my @sort = ( 'sort', '-r' );
		my @drop = ( 'mysql', @connectargs );
		run( \@dump, '|', \@grep, '|', \@sort, '|', \@drop, '>' ,\$ret, '2>', \$err )
			or $status = "Drop command returned $?:\n$err";
	} elsif( $opts{DBTYPE} eq 'SQLite' ) {
		say "Dropping SQLite database " . $opts{DBNAME};
		unlink( $opts{DBNAME} ) or $status = "Could not unlink SQLite file!";
	} else {
		$status = "Cannot currently reset DBs of type " . $opts{DBTYPE};
	}
	return $status;
}


1;