#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use TryCatch;

use_ok( 'Text::Tradition::Stemma' );

# Try to create a bad graph
TODO: {
	local $TODO = "cannot use stdout redirection trick with FastCGI";
	my $baddotfh;
	open( $baddotfh, 't/data/besoin_bad.dot' ) or die "Could not open test dotfile";
	try {
		my $stemma = Text::Tradition::Stemma->new( dot => $baddotfh );
		ok( 0, "Created broken stemma from dotfile with syntax error" );
	} catch( Text::Tradition::Error $e ) {
		like( $e->message, qr/^Error trying to parse/, "Syntax error in dot threw exception" );
	}
}

# Create a good graph
my $dotfh;
open( $dotfh, 't/data/florilegium.dot' ) or die "Could not open test dotfile";
binmode( $dotfh, ':utf8' );
my $stemma = Text::Tradition::Stemma->new( dot => $dotfh );
is( ref( $stemma ), 'Text::Tradition::Stemma', "Created stemma from good dotfile" );
is( scalar $stemma->witnesses, 13, "Found correct number of extant witnesses" );
is( scalar $stemma->hypotheticals, 8, "Found correct number of extant hypotheticals" );
ok( $stemma->has_identifier, "Stemma identifier was found in dot" );
is( $stemma->identifier, 'Coislinianum lineage', "Correct stemma identifier was found in dot" );
my $found_unicode_sigil;
foreach my $h ( $stemma->hypotheticals ) {
	$found_unicode_sigil = 1 if $h eq "\x{3b1}";
}
ok( $found_unicode_sigil, "Found a correctly encoded Unicode sigil" );

# TODO Create stemma from graph, create stemma from undirected graph,
# create stemma from incompletely-specified graph
}




1;
