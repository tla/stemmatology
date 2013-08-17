#!/usr/bin/perl -T

use strict;
use warnings;
use CGI;
use Encode qw/ decode /;
use Gearman::Client;
use JSON;
use Text::Tradition::Directory;
use Text::Tradition::Analysis::Result;
use TryCatch;

### Configurable variables
my %VARS = (
	DBTYPE => 'mysql',
	DBHOST => '127.0.0.1',
	DBPORT => '3006',
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

### Main program

my %status = (
    '400' => '400 Bad Request',
    '500' => '500 Internal Server Error',
);

my $q = CGI->new(\*STDIN);
# check that Content-Type is application/json
my $ctype = $q->content_type;
my $encoding = 'UTF-8'; # default
if( $ctype =~ m!^(\w+/[\w+]+);\s*charset=(.*)$! ) {
    ( $ctype, $encoding ) = ( $1, $2 );
}
error( 400, 'Content type must be application/json' ) 
        unless $ctype eq 'application/json';

# Get the post data, and decode it according to the given character set 
my $jsonstr = decode( $encoding, $q->param('POSTDATA') );
$jsonstr =~ s/\&/\n/g;
# Validate the JSON
my $request;
try {
    $request = from_json( $jsonstr );
} catch( $err ) {
    error( 400, "JSON parsing error: $err" );
}
# Request should be a hash that can be used to instantiate an Analysis::Result,
# or else an array of such hashes.
my @problems;
my $first = ref( $request ) eq 'ARRAY' ? shift @$request : $request;
try {
	my $result = Text::Tradition::Analysis::Result->new( $first );
	push( @problems, $result );
} catch( $err ) {
	error( 400, "Argument $first is neither a Result serialization nor an array: $err" );
}
# Now parse the rest of the result objects
unless( $first eq $request ) {
	foreach my $rj ( @$request ) {
		try {
			my $result = Text::Tradition::Analysis::Result->new( $rj );
			push( @problems, $result );
		} catch( $err ) {
			error( 400, "Argument $rj is not a Result serialization: $err" );
		}
	}
}

# For each of the result objects, see if its key exists in the DB. Kick off the
# calculation of any that need to be calculated, but don't wait more than two 
# seconds for a result. Return the DB version of each of the objects.
my $dbargs = {};
$dbargs->{user} = $VARS{DBUSER} if $VARS{DBUSER};
$dbargs->{password} = $VARS{DBPASS} if $VARS{DBPASS};
my $dir = Text::Tradition::Directory->new( 
	'dsn' => $VARS{DSN}, 'extra_args' => $dbargs );
my $scope = $dir->new_scope;
my %results;
my @resultorder;  # Keep track of the order in which we should return the results
my @needcalc;
foreach my $p ( @problems ) {
	my $key = $p->object_key;
	push( @resultorder, $key );
	my $result = $dir->lookup( $key );
	if( $result ) {
		$results{$key} = $result;
	} else {
		push( @needcalc, $p );
		$dir->store( $key => $p );
	}
}

# Now if any of the results need calculation, dispatch them for the purpose.
if( @needcalc ) {
	my $arg = join( ',', map { $_->object_key } @needcalc );
	my $client = Gearman::Client->new;
	$client->job_servers( $VARS{GEARMAN_SERVER} );
	my $task = $client->dispatch_background( run_idp => $arg );
	# See if it finishes quickly
	my $wait = 3;
    sleep( $wait );
	# Now replace the problems in the results hash with the DB results,
	# whether finished or still calculating.
	foreach my $p ( @needcalc ) {
		# this should NOT fail as we stored it above
		$results{$p->object_key} = $dir->lookup( $p->object_key );
	}
}

# Finally, assemble our answer.
my $answer;
if( $first eq $request ) {
	$answer = $results{$resultorder[0]};
} else {
	foreach my $key ( @resultorder ) {
		push( @$answer, $results{$key} );
	}
}


# Now return the response as UTF-8 encoded JSON.
print $q->header(-type => 'application/json', -charset => 'UTF-8' );
print JSON->new->allow_blessed->convert_blessed->utf8->encode( $answer );
exit 0;

sub error {
        my( $code, $msg ) = @_;
        print $q->header( -type => 'text/plain', -charset => 'UTF-8', -status => $status{$code} );
        print "$msg\n";
        exit 0;
}
