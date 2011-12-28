#!/usr/bin/env perl

use lib 'lib';
use strict;
use warnings;
use File::Basename;
use KiokuDB;
use KiokuDB::TypeMap::Entry::Naive;
use Text::Tradition;
use Text::Tradition::Stemma;

# Make a KiokuDB store from the traditions data we have.

my $kdb = KiokuDB->connect( "dbi:SQLite:dbname=db/traditions.db", 
	create => 1,
	typemap => KiokuDB::TypeMap->new(
            isa_entries => {
                "Graph" => KiokuDB::TypeMap::Entry::Naive->new,
                "Graph::AdjacencyMap" => KiokuDB::TypeMap::Entry::Naive->new,
            },
        ),
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
			open my $stemma_fh, '<', $stemmafile or die "Could not read stemma file $stemmafile";		
			$stemma = Text::Tradition::Stemma->new(
				'collation' => $tradition->collation,
				'dot' => $stemma_fh,
				);
		}
			
		my $scope = $kdb->new_scope;
		my $tid = $kdb->store( $tradition );
		my $sid = $kdb->store( $stemma ) if $stemma;
		print STDERR "Stored tradition for " . $tradition->name . " at $tid\n";
		print STDERR "\tand stemma at $sid\n" if $stemma;
	}
}

# Now try reading the objects from the DB.

my $scope = $kdb->new_scope;

my $stream = $kdb->root_set;
until( $stream->is_done ) {
	foreach my $t ( $stream->items ) {
		print STDERR "*** Object " . $kdb->object_to_id( $t ) . " ***\n";
		if( ref( $t ) eq 'Text::Tradition' ) {
			print STDERR "Got tradition " . $t->name . " out of the database\n";
			my @wits = map { $_->sigil } $t->witnesses;
			print STDERR "...with witnesses @wits\n";
			my $c = $t->collation;
			print STDERR "Collation has " . scalar( $c->readings ) . " readings\n";
			print STDERR "Collation has " . scalar( $c->paths ) . " paths\n";
			print STDERR "Collation has " . scalar( $c->relationships ) . " relationship links\n";
		} elsif( ref( $t ) eq 'Text::Tradition::Stemma' ) {
			print STDERR "Got stemma for tradition " . $t->collation->tradition->name 
				. " out of the database\n";
			print STDERR "Stemma graph is " . $t->graph . "\n";
		} else {
			print STDERR "Got unexpected object of type " . ref( $t ) 
				. " out of the database\n";
		}
	}
}