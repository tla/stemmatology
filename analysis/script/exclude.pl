#!/usr/bin/env perl

use feature 'say';
use lib 'lib';
use strict;
use warnings;
use Getopt::Long;
use Text::CSV_XS;
use Text::Tradition::Analysis qw/ run_analysis /;
use Text::Tradition::Directory;
use TryCatch;

binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';

my( $dsn, $dbuser, $dbpass );
my $filename = 'exclusions.csv';
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

## Set up the relationship types we will exclude in turn. False means "run 
## analysis with basic set of exclusions", i.e. orth/spelling/punct, and exclude
## the variants in question later. True means "explicitly exclude this type too
## at analysis time."
my @relation_types = qw/ none sameword grammatical lexical uncertain other
	addition deletion transposition /; 

# Set up the things we want to calculate for each text
my @calcs = qw/ total genealogical excoincidental reverted exgenealogical coincidental /;
my @resultfields = ( 'text_name' );
foreach my $rt ( @relation_types ) {
	foreach my $cc ( @calcs ) {
		push( @resultfields, sprintf( "%s_ex_%s", $cc, $rt ) );
	}
}
	
my $csv = Text::CSV_XS->new( { binary => 1, quote_null => 0 } );
open my $fh, ">:encoding(UTF-8)", $filename or die "$filename: $!";
if( $csv->combine( @resultfields ) ) {
	say $fh $csv->string;
} else {
	say "combine() failed on argument: " . $csv->error_input;
}

foreach my $tinfo( $dir->traditionlist ) {
	next if $tinfo->{'name'} eq 'xxxxx';
	next if $tinfo->{'name'} =~ /158/;
	next if $tinfo->{'name'} =~ /Heinrichi part/;
	if( $lookfor ) {
		next unless $tinfo->{'id'} eq $lookfor
			|| $tinfo->{'name'} =~ /$lookfor/;
	}
	my $tradition = $dir->lookup( $tinfo->{'id'} );
	next unless $tradition->stemma_count;
	say "Analyzing tradition " . $tradition->name;
	## HACK
	my $MAXRANK;
	if( $tradition->name =~ /Chronicle/ ) {
		$MAXRANK = $tradition->collation->reading('L1545')->rank;
	}
	my %datahash;
	# Initialize everything with zeroes
	map { $datahash{$_} = 0 } @resultfields;
	# Put in the real text ID and name
	$datahash{text_id} = $tinfo->{'id'};
	$datahash{text_name} = $tradition->name;
	
	my $fullresult;
	my $noorthresult;
	try {
		$fullresult = run_analysis( $tradition, exclude_type1 => 1 );
		$noorthresult = run_analysis( $tradition, exclude_type1 => 1,
			merge_types => [ qw/ orthographic spelling punctuation / ] );
	} catch {
		say "Analysis run failed on tradition " . $tradition->name . ": @_";
		return;
	}
	foreach my $rtype ( @relation_types ) {
		say "...calculating on exclusion of $rtype";
		my $result = $rtype eq 'none' ? $fullresult : $noorthresult;
			
		# Get the totals by location and by variant as we go.
		my $totalvariant = 0;
		my $singleton = 0;
		my $genvariant = 0;
		my $conflictvariant = 0;
		my $revertvariant = 0;
		my $msgd; # for the HACK
		my @unknown;
		foreach my $loc ( @{$result->{variants}} ) {
			# A transition is the relationship type between parent and child.
			# Find each genealogical transition
			# Find each non-genealogical transition
			if( exists $loc->{unsolved} ) {
				# Not solved; remove it from the total.
				say "Skipping unsolved location at " . $loc->{id};
				next;
			} elsif( $MAXRANK && $loc->{id} > $MAXRANK ) {
				# HACK until Chronicle tagging is done
				say "Skipping ranks above $MAXRANK"
					unless $msgd;
				$msgd = 1;
				next;
			}
			foreach my $rdghash( @{$loc->{readings}} ) {
				# Weed out singletons
				$totalvariant++;
				my @roots = @{$rdghash->{independent_occurrence}};
				if( @roots == 1 && !$rdghash->{'followed'} && !$rdghash->{'not_followed'}
					&& !$rdghash->{'follow_unknown'} ) {
					$singleton++;
					next;
				}
				my $type;
				if( $rdghash->{'is_conflict'} ) {
					$type = 'conflict';
				} elsif( $rdghash->{'is_reverted'} ) {
					$type = 'reverted';
				} elsif( @roots == 1 ) {
					$type = 'genealogical';
				} else {
					warn 'Reading ' . $rdghash->{readingid} . ' neither conflict, genealogical, nor reverted. What?';
					$type = 'ERROR';
				}
				# Get the relationship type stats for reading parents. 
				my $rdg = $tradition->collation->reading( $rdghash->{readingid} );

				my $phash = $type eq 'reverted' 
					? $rdghash->{'reversion_parents'} : $rdghash->{'source_parents'};
				my @rels = _get_reading_relations( $rdghash->{'readingid'}, $loc->{'id'},
					$rdg, $type, $phash, \%datahash, \@unknown );
				# If this is one of our exclusions, take it out of the total.
				foreach my $rel ( @rels ) {
					if( $rel eq $rtype ) {
						$totalvariant--;
						next;
					} else {
				# Otherwise add the variant type to our count.
						$conflictvariant++ if $type eq 'conflict';
						$revertvariant++ if $type eq 'reverted';
						$genvariant++ if $type eq 'genealogical';
					}
				}
			}
		}
	
		# Add in the sums for the whole location
		$datahash{"total_ex_$rtype"} = $totalvariant - $singleton;
		$datahash{"genealogical_ex_$rtype"} = $genvariant;
		$datahash{"reverted_ex_$rtype"} = $revertvariant;
		$datahash{"coincidental_ex_$rtype"} = $conflictvariant;
		$datahash{"excoincidental_ex_$rtype"} = $genvariant + $revertvariant;
		$datahash{"exgenealogical_ex_$rtype"} = $conflictvariant + $revertvariant;
	}
	
	# Write them out to CSV.
	my @csvalues = map { $datahash{$_} } @resultfields;
	if( $csv->combine( @csvalues ) ) {
		say $fh $csv->string;
	} else {
		say "combine() failed on argument: " . $csv->error_input;
	}
}

close $fh;

sub _get_reading_relations {
	my( $rid, $rank, $robj, $type, $parenthash, $datahash, $unknown ) = @_;
	my @kp = keys ( %$parenthash );
	return ( 'source' ) unless @kp; # In case there is no parent reading to relate.

	my @rels;
	foreach my $p ( @kp ) {
		my $pdata = $parenthash->{$p};
		my $relation;
		if( $pdata->{relation} ) {
			$relation = $pdata->{relation}->{transposed}
				? 'transposition' : $pdata->{relation}->{type};
		} else {
			$relation = 'unknown';
			if( !$robj ) {
				say "Unknown relation on missing reading object $rid at rank $rank";
			} elsif( !$pdata ) {
				say "Unknown relation on missing parent object for $rid at rank $rank";			
			} else {
				push( @$unknown, [ $pdata->{label}, $robj->id, $robj->text, $type ] );
			}
		}
		push( @rels, $relation );
	}
	return @rels;
}

