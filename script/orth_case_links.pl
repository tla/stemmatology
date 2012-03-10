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
	my $scope = $dir->new_scope;
	my $tradition = $dir->lookup( $id );
	print STDERR "Processing tradition " . $tradition->name . "\n";
	my $c = $tradition->collation;
	foreach my $rank ( 1 .. $c->end->rank - 1 ) {
		my @readings = $c->readings_at_rank( $rank );
		while( @readings ) {
			my $r = pop @readings;
			next if $r->is_meta;
			my @orthmatch = grep { lc( $r->text ) eq lc( $_->text ) } @readings;
			foreach my $om ( @orthmatch ) {
				unless( $c->get_relationship( $r, $om ) ) {
					print STDERR sprintf( "Adding orthographic link for %s / %s\n", 
						$r->text, $om->text );
					$DB::single = 1;
					$c->add_relationship( $r, $om, 
						{ 'type' => 'orthographic', 'scope' => 'global' } );
				}
			}
		}		
	}
	$dir->save( $tradition );
}

print STDERR "Done\n";
