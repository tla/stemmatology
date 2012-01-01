#!/usr/bin/env perl

use lib 'lib';
use strict;
use warnings;
use File::Basename;
use Text::Tradition;
use Text::Tradition::Directory;
use Text::Tradition::Stemma;

binmode( STDOUT, ':utf8' );
binmode( STDERR, ':utf8' );

# Make a KiokuDB store from the traditions data we have.

my $kdb = Text::Tradition::Directory->new(
	'dsn' => "dbi:SQLite:dbname=db/traditions.db",
	'extra_args' => { 'create' => 1 },
    );
    
my %stemma_map = (
	'florilegium.xml' => 'stemma_a.dot',
	'besoin.xml' => 'stemma_b.dot',
	'heinrichi.xml' => 'stemma_h.dot',
	'parzival.xml' => 'stemma_p.dot',
	's158.xml' => 'stemma_s.dot',
	);

my $dir = $ARGV[0];
if( $dir ) {
	$dir =~ s/\/$//;
	opendir( DIR, $dir ) or die "Could not open directory $dir";
	while( readdir DIR ) {
		next unless /\.xml$/;
		print STDERR "Looking at $_\n";
		my $tradition = Text::Tradition->new( 
			'input' => 'Self',
			'file' => "$dir/$_",
			'linear' => 1,
			);
		my $stemma;
		if( exists $stemma_map{$_} ) {
			my $stemmafile = "$dir/" . $stemma_map{$_};	
			$stemma = $tradition->add_stemma( $stemmafile );
		}
		my $scope = $kdb->new_scope();
		my $tid = $kdb->save( $tradition );
		print STDERR "Stored tradition for " . $tradition->name . " at $tid\n";
	}
}

# Now try reading the objects from the DB.
foreach my $tid ( $kdb->tradition_ids ) {
	my $scope = $kdb->new_scope();
	my $t = $kdb->tradition( $tid );
	print STDERR "Got tradition " . $t->name . " out of the database\n";
	my @wits = map { $_->sigil } $t->witnesses;
	print STDERR "...with witnesses @wits\n";
	my $c = $t->collation;
	print STDERR "Collation has " . scalar( $c->readings ) . " readings\n";
	print STDERR "Collation has " . scalar( $c->paths ) . " paths\n";
	print STDERR "Collation has " . scalar( $c->relationships ) . " relationship links\n";
	my $s = $t->stemma;
	if( $s ) {
		print STDERR "Got stemma for tradition " . $s->collation->tradition->name 
			. " out of the database\n";
		print STDERR "Stemma graph is " . $s->graph . "\n";
	}
}
