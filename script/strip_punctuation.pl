#!/usr/bin/env perl

use lib 'lib';
use strict;
use warnings;
use Text::Tradition::Directory;

binmode STDERR, ':utf8';

my( $dsn, $user, $pass ) = @ARGV;

my $connect_args = { dsn => $dsn };
$connect_args->{'extra_args'} = { user => $user, password => $pass }
	if $user && $pass;
my $dir = Text::Tradition::Directory->new( $connect_args );

foreach my $id ( $dir->tradition_ids ) {
	my $scope = $dir->new_scope;
	my $tradition = $dir->lookup( $id );
	print STDERR "Processing tradition " . $tradition->name . "\n";
	foreach my $reading ( $tradition->collation->readings ) {
		next if $reading->is_meta;
		$reading->alter_text( strip_punct( $reading->text ) );
	}
	$tradition->collation->flatten_ranks;
	$dir->save( $tradition );
}

print STDERR "Done\n";

sub strip_punct {
	my( $rtext ) = @_;
	my $orig_r = $rtext;
	return $rtext unless $rtext =~ /\w/;
	$rtext =~ s/^\W+//;
	$rtext =~ s/\W+$//;
	print STDERR "Altering $orig_r to $rtext\n"
		unless $orig_r eq $rtext;
	return $rtext;
}
	