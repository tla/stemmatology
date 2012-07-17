#!/usr/bin/env perl

use feature 'say';
use lib 'lib';
use strict;
use warnings;
use Text::CSV_XS;
use Text::Tradition::Analysis qw/ run_analysis /;
use Text::Tradition::Directory;

binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';

my $dir = Text::Tradition::Directory->new(
    'dsn' => 'dbi:SQLite:dbname=stemmaweb/db/traditions.db',
    );

my $scope = $dir->new_scope();
my $lookfor = shift @ARGV || '';
my $collapse = [ @ARGV ];

my @relation_types = qw/ orthographic spelling grammatical lexical
	transposition addition deletion wordsimilar unknown /;

my @resultfields = qw/
	text_name loc_total loc_totalvariant loc_genealogical loc_genvariant 
	loc_conflict loc_conflictvariant percent_genealogical percent_genvariant /;
map { push( @resultfields, "gen_$_", "gen_${_}_nonsense", "gen_${_}_ungramm" ) }
	@relation_types;
map { push( @resultfields, "con_$_", "con_${_}_nonsense", "con_${_}_ungramm" ) }
	@relation_types;
map { push( @resultfields, "percent_gen_$_", "percent_${_}_gen" ) } @relation_types;
map { push( @resultfields, "percent_con_$_" ) } @relation_types;
	
my $csv = Text::CSV_XS->new( { binary => 1, quote_null => 0 } );
open my $fh, ">:encoding(UTF-8)", "analysis.csv" or die "analysis.csv: $!";
if( $csv->combine( @resultfields ) ) {
	say $fh $csv->string;
} else {
	say "combine() failed on argument: " . $csv->error_input;
}

foreach my $tinfo( $dir->traditionlist ) {
	next if $tinfo->{'name'} eq 'xxxxx';
	next unless $tinfo->{'id'} eq $lookfor
		|| $tinfo->{'name'} =~ /$lookfor/;
	my $tradition = $dir->lookup( $tinfo->{'id'} );
	say "Analyzing tradition " . $tradition->name;
	my %datahash;
	# Initialize everything with zeroes
	map { $datahash{$_} = 0 } @resultfields;
	# Put in the real text ID and name
	$datahash{text_id} = $tinfo->{'id'};
	$datahash{text_name} = $tradition->name;
	
	# Run the analysis for each row in @rows
	my %opts = ( exclude_type1 => 1 );
	if( @$collapse ) {
		$opts{merge_types} = $collapse;
	}
	
	my $result = run_analysis( $tradition, %opts );
	$datahash{loc_total} = $result->{variant_count};
	$datahash{loc_genealogical} = $result->{genealogical_count};
	$datahash{loc_conflict} = $result->{variant_count} - $result->{genealogical_count};
	$datahash{loc_conflictvariant} = $result->{conflict_count};
	# Get the number of total and genealogical variants as we go below.
	my $totalvariant = 0;
	my $genvariant = 0;
	my @unknown;
	foreach my $loc ( @{$result->{variants}} ) {
		# A transition is the relationship type between parent and child.
		# Find each genealogical transition and get the relationship type (if any)
		# Find each non-genealogical transition and get the relationship type (if any)
		# Someday, look for reversions
		foreach my $rdghash( @{$loc->{readings}} ) {
			# Weed out singletons
			my @roots = @{$rdghash->{independent_occurrence}};
			next if @roots == 1 && !$rdghash->{'followed'} && !$rdghash->{'not_followed'}
				&& !$rdghash->{'follow_unknown'};
			# TODO Weed out punctuation
			my $rdg = $tradition->collation->reading( $rdghash->{readingid} );
			my $typekey = @roots == 1 ? 'gen_' : 'con_';
			foreach my $p ( keys %{$rdghash->{reading_parents}} ) {
				my $pdata = $rdghash->{reading_parents}->{$p};
				my $relation;
				if( $pdata->{relation} ) {
					$relation = $pdata->{relation}->{type};
				} else {
					$relation = 'unknown';
					if( !$rdg ) {
						say "Unknown relation on missing reading object " 
							. $rdghash->{readingid} . " at rank " . $loc->{id};
					} elsif( !$pdata ) {
						say "Unknown relation on missing parent object for " 
							. $rdghash->{readingid} . " at rank " . $loc->{id};
					
					} else {
						push( @unknown, [ $pdata->{label}, $rdg->id, $rdg->text, 
							( @roots == 1 ? 'genealogical' : 'conflicting' ) ] );
					}
				}
				$typekey .= $relation;
				$datahash{$typekey}++;
				## TODO distinguish parent-bad vs. rdg-bad
				if( $rdg && $rdg->grammar_invalid ) {
					$datahash{$typekey.'_ungramm'} = 1;
				} elsif( $rdg && $rdg->is_nonsense ) {
					$datahash{$typekey.'_nonsense'} = 1;
				}
			}
			$totalvariant++;
			$genvariant++ if @roots == 1;
		}
	}
	
	# Add in the sums for the whole location
	$datahash{'loc_genvariant'} = $genvariant;	
	$datahash{'loc_totalvariant'} = $totalvariant;
	$datahash{'percent_genealogical'} = $datahash{loc_genealogical} / $datahash{loc_total};
	$datahash{'percent_genvariant'} = $genvariant / $totalvariant;
	foreach my $type ( @relation_types ) {
		$datahash{"percent_gen_$type"} = $datahash{"gen_$type"} / $totalvariant;
		$datahash{"percent_con_$type"} = $datahash{"con_$type"} / $totalvariant;
		$datahash{"percent_${type}_gen"} = 
			$datahash{"gen_$type"} + $datahash{"con_$type"} == 0 ? 0 :
			$datahash{"gen_$type"} / ( $datahash{"gen_$type"} + $datahash{"con_$type"} );
	}
	
	# Write them out to CSV.
	my @csvalues = map { $datahash{$_} } @resultfields;
	if( $csv->combine( @csvalues ) ) {
		say $fh $csv->string;
	} else {
		say "combine() failed on argument: " . $csv->error_input;
	}
	map { say sprintf( "Unknown transition %s -> %s (%s) :%s", @$_ ) } @unknown;
}

close $fh;
