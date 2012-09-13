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
	my $tradition = $dir->lookup( $tinfo->{id} );
	say STDERR "Processing tradition " . $tradition->name;
	my $c = $tradition->collation;
	$c->flatten_ranks(); # just in case
	foreach my $rank ( 1 .. $c->end->rank - 1 ) {
		my @readings = $c->readings_at_rank( $rank );
		my %merged;
		while( @readings ) {
			my $r = pop @readings;
			next if $r->is_meta;
			next if $merged{$r->id};
			my @orthmatch = grep { lc( $r->text ) eq lc( $_->text ) } @readings;
			foreach my $om ( @orthmatch ) {
				if( $r->text eq $om->text ) {
					say STDERR "Merging identical readings $r and $om (" 
						. $r->text . ")";
					$merged{$om->id} = 1;
					$c->merge_readings( $r, $om ); 
				} else {
					say STDERR sprintf( "Adding orthographic link for %s and %s (%s / %s)", 
						$r->id, $om->id, $r->text, $om->text );
					try { 
						$c->add_relationship( $r, $om, 
							{ 'type' => 'orthographic', 'scope' => 'global' } ); };
					} catch ( Text::Tradition::Error $e ) {
						say STDERR "Relationship skipped: " . $e->message;
					}
				}
			}
		}		
	}
	$dir->save( $tradition );
}

print STDERR "Done\n";
