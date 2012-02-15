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

my( $tfile, $format, $sfile, $delete, $list, $dsn ) = 
	( undef, 'Self', undef, undef, 0, 'dbi:SQLite:dbname=db/traditions.db' );

GetOptions( 
	't|tradition=s' => \$tfile,
	'f|format=s' => \$format,
	's|stemma=s' => \$sfile,
	'l|list' => \$list,
	'd|delete=s' => \$delete,
	'dsn=s' => \$dsn,
	);

# Make a KiokuDB store from the traditions data we have.

my $kdb = Text::Tradition::Directory->new(
	'dsn' => $dsn,
	'extra_args' => { 'create' => 1 },
    );
    
unless( $tfile || $delete || $list ) {
	print STDERR "Please specify a tradition file, an ID to delete, or the --list option\n";
	exit;
}

if( $tfile && $delete ) {
	print STDERR "Specify deletion by UUID, not by tradition file\n";
	exit;
}

my( $tradition, $stemma );
if( $tfile ) {
	print STDERR "Reading tradition from $tfile\n";
	$tradition = Text::Tradition->new( 
		'input' => $format,
		'file' => $tfile,
		'linear' => 1,
		);
	if( $tradition && $sfile ) {
		$stemma = $tradition->add_stemma( dotfile =>  $sfile );
		warn "Did not get stemma from $sfile\n" unless $stemma;
	}
    
	my $scope = $kdb->new_scope();
	my $tid = $kdb->save( $tradition );
	print STDERR "Stored tradition for " . $tradition->name . " at $tid\n";
	print STDERR "...and associated stemma from $sfile\n" if $stemma;
}

if( $delete ) {
	my $scope = $kdb->new_scope();
	if( $kdb->exists( $delete ) ) {
		$kdb->delete( $delete );
	} else {
		print STDERR "Object $delete does not appear to be a Text::Tradition in the DB\n";
	}
}

# Now try reading the objects from the DB.
if( $list ) {
	foreach my $tid ( $kdb->tradition_ids ) {
		my $scope = $kdb->new_scope();
		my $t = $kdb->tradition( $tid );
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