#!/usr/bin/env perl

use lib 'lib';
use feature 'say';
use strict;
use warnings;
use Text::Tradition;
use Text::Tradition::Directory;
use TryCatch;

binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';

my $dir = Text::Tradition::Directory->new(
    'dsn' => 'dbi:SQLite:dbname=db/traditions.db',
    );

my $scope = $dir->new_scope();
my $main = Text::Tradition->new( name => 'Heinrichi combined', language => 'Finnish' );
$main->_init_done( 0 ); # so that we can add disconnected readings
my $mc = $main->collation;

my $first = $ARGV[0];
my $last = $ARGV[-1];
my $prior;
my @endpaths;
foreach my $id ( @ARGV ) {
	my $tradition = $dir->lookup( $id );
	say STDERR "Applying readings from " . $tradition->name;
	my $c = $tradition->collation;
	
	## Duplicate the witnesses
	foreach my $wit ( $tradition->witnesses ) {
		unless( $main->has_witness( $wit ) ) {
			my %witopts = (
				sigil => $wit->sigil,
				sourcetype => $wit->sourcetype,
				is_layered => $wit->is_layered,
				is_collated => $wit->is_collated 
				);
			$main->add_witness( %witopts );
		}
	}
	
	## Duplicate the readings
	foreach my $rdg ( $c->readings ) {
		my %rdg_opts;
		my $skip;
		
		## If the reading exists already, just check consistency.
		if( $mc->reading( $rdg->id ) ) {
			# Check that it matches
			die "Reading mismatch at $rdg" unless $rdg->text eq $mc->reading( $rdg->id )->text;
		} else {
			# Create the new reading
			%rdg_opts = ( 
				id => $rdg->id,
				text => $rdg->text,
				is_lacuna => $rdg->is_lacuna,
				is_common => $rdg->is_common,
				rank => $rdg->rank,
				join_prior => $rdg->join_prior,
				join_next => $rdg->join_next ) 
				unless keys( %rdg_opts );
			$mc->add_reading( \%rdg_opts );
		}
	}
}
	
## Duplicate the edges
my %need_end;
foreach my $id ( @ARGV ) {
	my $tradition = $dir->lookup( $id );
	say STDERR "Applying edges from " . $tradition->name;
	my $c = $tradition->collation;
	foreach my $p ( sort { _by_source_rank( $a, $b, $c ) } $c->paths ) {
		foreach my $wit ( $c->path_witnesses( $p ) ) {
			my @vector = @$p;
			# Don't connect to intermediate start/end nodes
			if( $id ne $first && $c->reading( $p->[0] )->is_start ) {
				if( exists $need_end{$wit} ) {
					# Connect them
					my $start = delete $need_end{$wit};
					next if $start eq $p->[1];
					@vector = ( $start, $p->[1] );
				} else {
					warn "Unconnected second half of path for $wit at " . $p->[1];
					# Try connecting it to the start node
					@vector = ( $mc->start, $p->[1] );
				}
			}
			if ( $id ne $last && $c->reading( $p->[1] )->is_end ) {
				$need_end{$wit} = $p->[0];
				next;
			} 
			unless( $mc->has_path( @vector, $wit ) ) {
				$mc->add_path( @vector, $wit );
			}
		}
	}
	if( $id eq $last ) {
		# Connect whatever is left in $need_end
		foreach my $wit ( keys %need_end ) {
			my $start = delete $need_end{$wit};
			$mc->add_path( $start, $mc->end, $wit );
		}
	}
}

## Make the witness text and indicate that the paths are in place
$mc->text_from_paths();
$main->_init_done( 1 );
$mc->calculate_ranks();

## Apply the relationships
foreach my $id ( @ARGV ) {
	my $tradition = $dir->lookup( $id );
	say STDERR "Applying relationships from " . $tradition->name;
	my $c = $tradition->collation;
	## Duplicate the relationships
	foreach my $reledge ( sort {
			_apply_relationship_order( $a, $b, $c ) } $c->relationships ) {
		my $rel = $c->get_relationship( $reledge );
		# Apply it
		try {
			apply_relationship( $rel, $reledge );
		} catch( Text::Tradition::Error $err ) {
			say STDERR "Failed to apply " . $rel->type . " rel at @$reledge: "
				. $err->message;
		}
	}
}

$mc->calculate_ranks();
$mc->flatten_ranks();
say $mc->as_graphml();

sub apply_relationship {
	my( $rel, $reledge ) = @_;
	my $opts = {
		type => $rel->type,
		scope => $rel->scope };
	$opts->{annotation} = $rel->annotation if $rel->has_annotation;
	$opts->{thispaironly} = 1 if $rel->nonlocal;
	unless( $mc->get_relationship( $reledge ) ) {
		$mc->add_relationship( @$reledge, $opts );
	}
}

# Helper function for applying the path edges in the expected order
sub _by_source_rank {
	my( $a, $b, $c ) = @_;
	return $c->reading( $a->[0] )->rank <=> $c->reading( $b->[0] )->rank;
}

# Helper sort function for applying the saved relationships in a
# sensible order.
sub _apply_relationship_order {
	my( $a, $b, $c ) = @_;
	my $rg = $mc->relations;
	my $aobj = $c->get_relationship( $a ); my $bobj = $c->get_relationship( $b );
	my $at = $rg->type( $aobj->type ); my $bt = $rg->type( $bobj->type );
	# Apply strong relationships before weak
	return -1 if $bt->is_weak && !$at->is_weak;
	return 1 if $at->is_weak && !$bt->is_weak;
	# Apply local before global
	return -1 if !$aobj->nonlocal && $bobj->nonlocal;
	return 1 if !$bobj->nonlocal && $aobj->nonlocal;
	# Apply more tightly bound relationships first
	return $at->bindlevel <=> $bt->bindlevel;
}
