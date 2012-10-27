#!/usr/bin/env perl

use feature 'say';
use lib 'lib';
use strict;
use warnings;
use Getopt::Long;
use Set::Scalar;
use Text::CSV_XS;
use Text::Tradition::Analysis qw/ group_variants /;
use Text::Tradition::Directory;
use TryCatch;

binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';

my( $dsn, $dbuser, $dbpass );
my $filename = 'magnitude.csv';
GetOptions(
	'dsn=s' => \$dsn,
	'u|user=s'   => \$dbuser,
	'p|pass=s' => \$dbpass,
	'f|file=s' => \$filename
);

my %dbopts = ( dsn => $dsn );
if( $dbuser || $dbpass ) {
	$dbopts{extra_args} = { user => $dbuser, password => $dbpass }
}

my $dir = Text::Tradition::Directory->new( %dbopts );
my $scope = $dir->new_scope();
my $lookfor = shift @ARGV || '';
my $collapse = Set::Scalar->new();
if( @ARGV ) {
	say "Merging relationship types @ARGV";
	map { $collapse->insert($_) } @ARGV;
}

my $csv = Text::CSV_XS->new( { binary => 1, quote_null => 0 } );
open my $fh, ">:encoding(UTF-8)", $filename or die "$filename: $!";

foreach my $tinfo( $dir->traditionlist ) {
	next if $tinfo->{'name'} eq 'xxxxx';
	next if $tinfo->{'name'} =~ /158/;
	next if $tinfo->{'name'} =~ /Heinrichi part/;
	if( $lookfor ) {
		next unless $tinfo->{'id'} eq $lookfor
			|| $tinfo->{'name'} =~ /$lookfor/;
	}
	my $tradition = $dir->lookup( $tinfo->{'id'} );
	say "Counting variation in tradition " . $tradition->name;
    
    # Group the variants for each rank, and count the number of
    # reading groupings.
    my $lcph = Set::Scalar->new(); # placeholder for lacunae
    my $moved = {};
    my %magnitudes;
    my $max = 1;
    foreach my $rk ( 1 .. $tradition->collation->end->rank ) {
        my $missing = $lcph->clone();
        my $rankgroup = group_variants( $tradition, $rk, $missing, $moved, $collapse );
		my $numr = scalar keys %$rankgroup;
        $numr++ if $missing->size;
        $max = $numr if $numr > $max;
		if( exists $magnitudes{$numr} ) {
			$magnitudes{$numr}++
		} else {
			$magnitudes{$numr} = 1;
        }
    }
    
	# Write them out to CSV.
	my @csvalues = map { $magnitudes{$_} || 0 } 2..$max;
	if( $csv->combine( $tradition->name, @csvalues ) ) {
		say $fh $csv->string;
	} else {
		say "combine() failed on argument: " . $csv->error_input;
	}
}

close $fh;
