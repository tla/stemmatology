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
GetOptions(
	'dsn=s' => \$dsn,
	'u|user=s'   => \$dbuser,
	'p|pass=s' => \$dbpass,
);

my %dbopts = ( dsn => $dsn );
if( $dbuser || $dbpass ) {
	$dbopts{extra_args} = { user => $dbuser, password => $dbpass }
}

my $dir = Text::Tradition::Directory->new( %dbopts );
my $scope = $dir->new_scope();
my $lookfor = shift @ARGV || '';
my %collapse;
if( @ARGV ) {
	say "Merging relationship types @ARGV";
	map { $collapse{$_} = 1 } @ARGV;
}

my @relation_types = grep { !$collapse{$_} }
	qw/ orthographic spelling grammatical lexical transposition repetition
	    uncertain other addition deletion wordsimilar unknown /;

my @resultfields = qw/
	text_name loc_total loc_totalvariant loc_genealogical loc_genvariant 
	loc_conflict loc_conflictvariant loc_reverted loc_revertvariant 
	percent_genealogical percent_genvariant percent_genorrevert /;
map { push( @resultfields, "gen_$_", "con_$_", "rev_$_" ) } @relation_types;
# map { push( @resultfields, "gen_$_", "gen_${_}_nonsense", "gen_${_}_ungramm" ) }
# 	@relation_types;
# map { push( @resultfields, "con_$_", "con_${_}_nonsense", "con_${_}_ungramm" ) }
# 	@relation_types;
# map { push( @resultfields, "rev_$_", "rev_${_}_nonsense", "rev_${_}_ungramm" ) }
# 	@relation_types;
#map { push( @resultfields, "percent_gen_$_", "percent_${_}_gen", 
#			"percent_${_}_notcon" ) } @relation_types;
#map { push( @resultfields, "percent_con_$_", "percent_rev_$_", 
#			"percent_noncon_$_" ) } @relation_types;
	
my $csv = Text::CSV_XS->new( { binary => 1, quote_null => 0 } );
open my $fh, ">:encoding(UTF-8)", "analysis.csv" or die "analysis.csv: $!";
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
	
	# Run the analysis for each row in @rows
	my %opts = ( 
		exclude_type1 => 1,
		merge_types => [ 'punctuation' ] );
	if( keys %collapse ) {
		push( @{$opts{merge_types}}, keys %collapse );
	}
	
	my $result;
	#try {
		$result = run_analysis( $tradition, %opts );
	#} catch {
	#	say STDERR "Analysis run failed on tradition " . $tradition->name . ": @_";
	#	next;
	#}
	$datahash{loc_total} = $result->{variant_count};
	$datahash{loc_genealogical} = $result->{genealogical_count};
	$datahash{loc_conflictvariant} = $result->{conflict_count};
	$datahash{loc_revertvariant} = $result->{reversion_count};
	# Get the number of total and genealogical variants, and number of
	# conflicted/reverted locations, as we go below.
	my $totalvariant = 0;
	my $genvariant = 0;
	my $conflictloc = 0;
	my $revertloc = 0;
	my $msgd;
	my @unknown;
	foreach my $loc ( @{$result->{variants}} ) {
		# A transition is the relationship type between parent and child.
		# Find each genealogical transition and get the relationship type (if any)
		# Find each non-genealogical transition and get the relationship type (if any)
		my( $loc_conflict, $loc_reversion );
		if( exists $loc->{unsolved} ) {
			say STDERR "Skipping unsolved location at " . $loc->{id};
			next;
		} elsif( $MAXRANK && $loc->{id} > $MAXRANK ) {
			say STDERR "Skipping ranks above $MAXRANK"
				unless $msgd;
			$msgd = 1;
			next;
		}
		foreach my $rdghash( @{$loc->{readings}} ) {
			# Weed out singletons
			my @roots = @{$rdghash->{independent_occurrence}};
			next if @roots == 1 && !$rdghash->{'followed'} && !$rdghash->{'not_followed'}
				&& !$rdghash->{'follow_unknown'};
			my $rdg = $tradition->collation->reading( $rdghash->{readingid} );
			my $type;
			if( $rdghash->{'is_conflict'} ) {
				$type = 'conflict';
				$loc_conflict = 1;
			} elsif( $rdghash->{'is_reverted'} ) {
				$type = 'reverted';
				$loc_reversion = 1;
			} elsif( @roots == 1 ) {
				$type = 'genealogical';
				$genvariant++;
			} else {
				warn "Reading $rdg neither conflict, genealogical, nor reverted. What?";
				$type = 'ERROR';
			}
			my $typekey = substr( $type, 0, 3 ) . '_';
			
			# Add relation stats for reading parents. If the reading is reverted,
			# treat it as genealogical for the parent.
			_add_reading_relations( $rdghash->{'readingid'}, $loc->{'id'}, $rdg,
				( $type eq 'reverted' ? 'genealogical' : $type ),
				$rdghash->{'source_parents'}, \%datahash, \@unknown );
			# Add relation stats for reading reversions if they exist.
			if( $type eq 'reverted' ) {
				# Get relationship between reverted readings and their non-matching
				# parents.
				_add_reading_relations( $rdghash->{'readingid'}, $loc->{'id'}, $rdg,
					$type, $rdghash->{'reversion_parents'}, \%datahash, \@unknown );
			}
			
			$totalvariant++;
		}
		if( $loc_conflict ) {
			$conflictloc++;
		} elsif( $loc_reversion ) {
			$revertloc++;
		}
	}
	
	# Add in the sums for the whole location
	$datahash{'loc_genvariant'} = $genvariant;	
	$datahash{'loc_totalvariant'} = $totalvariant;
	$datahash{'loc_conflict'} = $conflictloc;
	$datahash{'loc_reverted'} = $revertloc;
	$datahash{'percent_genealogical'} = $datahash{loc_genealogical} / $datahash{loc_total};
	$datahash{'percent_genvariant'} = $genvariant / $totalvariant;
	$datahash{'percent_genorrevert'} = ( $genvariant + $datahash{loc_revertvariant} ) / $totalvariant;
	foreach my $type ( @relation_types ) {
		my $pgtype = $datahash{"gen_$type"};
		my $pctype = $datahash{"con_$type"};
		my $prtype = $datahash{"rev_$type"};
	#	$datahash{"percent_gen_$type"} = $pgtype / $totalvariant;
	#	$datahash{"percent_con_$type"} = $pctype / $totalvariant;
	#	$datahash{"percent_rev_$type"} = $prtype / $totalvariant;
	#	$datahash{"percent_notcon_$type"} = ( $pgtype + $prtype ) / $totalvariant;
	#	$datahash{"percent_${type}_gen"} = 
	#		$pgtype + $pctype + $prtype == 0 
	#			? 0 : $pgtype / ( $pgtype + $pctype + $prtype );
	#	$datahash{"percent_${type}_notcon"} = 
	#		$pgtype + $pctype + $prtype == 0 
	#			? 0 : ( $pgtype + $prtype ) / ( $pgtype + $pctype + $prtype );
	}
	
	# Write them out to CSV.
	my @csvalues = map { $datahash{$_} } @resultfields;
	if( $csv->combine( @csvalues ) ) {
		say $fh $csv->string;
	} else {
		say "combine() failed on argument: " . $csv->error_input;
	}
	map { say STDERR sprintf( "Unknown transition %s -> %s (%s) :%s", @$_ ) } @unknown;
}

close $fh;

sub _add_reading_relations {
	my( $rid, $rank, $robj, $type, $parenthash, $datahash, $unknown ) = @_;
	foreach my $p ( keys %$parenthash ) {
		my $pdata = $parenthash->{$p};
		my $relation;
		if( $pdata->{relation} ) {
			$relation = $pdata->{relation}->{type};
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
		my $typekey = substr( $type, 0, 3 ) . "_$relation";
		$datahash->{$typekey}++;
		## TODO distinguish parent-bad vs. rdg-bad
# 		if( $robj && $robj->grammar_invalid ) {
# 			$datahash->{$typekey.'_ungramm'} = 1;
# 		} elsif( $robj && $robj->is_nonsense ) {
# 			$datahash->{$typekey.'_nonsense'} = 1;
# 		}
	}
}
