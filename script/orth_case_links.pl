#!/usr/bin/env perl

use lib 'lib';
use strict;
use warnings;
use Text::Tradition::Directory;

binmode STDERR, ':utf8';
binmode STDOUT, ':utf8';
eval { no warnings; binmode $DB::OUT, ':utf8' };

my( $dsn, $user, $pass ) = @ARGV;

my $connect_args = { dsn => $dsn };
$connect_args->{'extra_args'} = { user => $user, password => $pass }
	if $user && $pass;
my $dir = Text::Tradition::Directory->new( $connect_args );

foreach my $text ( $dir->traditionlist ) {
	my $id = $text->{'id'};
	next unless $text->{'name'} =~ /Heinrichi/;
	my $scope = $dir->new_scope;
	my $tradition = $dir->lookup( $id );
	print STDERR "Processing tradition " . $tradition->name . "\n";
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
					print STDERR "Merging identical readings $r and $om (" 
						. $r->text . ")\n";
					$merged{$om->id} = 1;
					$c->merge_readings( $r, $om ); 
				} else {
					print STDERR sprintf( "Adding orthographic link for %s and %s (%s / %s)\n", 
						$r->id, $om->id, $r->text, $om->text );
					eval { $c->add_relationship( $r, $om, 
						{ 'type' => 'orthographic', 'scope' => 'global' } ); };
					print STDERR $@ if $@;
				}
			}
		}		
	}
	$dir->save( $tradition );
}

print STDERR "Done\n";
