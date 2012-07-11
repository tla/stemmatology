package Text::Tradition::Collation::RelationshipStore;

use strict;
use warnings;
use Text::Tradition::Error;
use Text::Tradition::Collation::Relationship;
use TryCatch;

use Moose;

=head1 NAME

Text::Tradition::Collation::RelationshipStore - Keeps track of the relationships
between readings in a given collation
    
=head1 DESCRIPTION

Text::Tradition is a library for representation and analysis of collated
texts, particularly medieval ones.  The RelationshipStore is an internal object
of the collation, to keep track of the defined relationships (both specific and
general) between readings.

=begin testing

use Text::Tradition;
use TryCatch;

use_ok( 'Text::Tradition::Collation::RelationshipStore' );

# Add some relationships, and delete them

my $cxfile = 't/data/Collatex-16.xml';
my $t = Text::Tradition->new( 
	'name'  => 'inline', 
	'input' => 'CollateX',
	'file'  => $cxfile,
	);
my $c = $t->collation;

my @v1 = $c->add_relationship( 'n21', 'n22', { 'type' => 'lexical' } );
is( scalar @v1, 1, "Added a single relationship" );
is( $v1[0]->[0], 'n21', "Got correct node 1" );
is( $v1[0]->[1], 'n22', "Got correct node 2" );
my @v2 = $c->add_relationship( 'n24', 'n23', 
	{ 'type' => 'spelling', 'scope' => 'global' } );
is( scalar @v2, 2, "Added a global relationship with two instances" );
@v1 = $c->del_relationship( 'n22', 'n21' );
is( scalar @v1, 1, "Deleted first relationship" );
@v2 = $c->del_relationship( 'n12', 'n13' );
is( scalar @v2, 2, "Deleted second global relationship" );
my @v3 = $c->del_relationship( 'n1', 'n2' );
is( scalar @v3, 0, "Nothing deleted on non-existent relationship" );

=end testing

=head1 METHODS

=head2 new( collation => $collation );

Creates a new relationship store for the given collation.

=cut

has 'collation' => (
	is => 'ro',
	isa => 'Text::Tradition::Collation',
	required => 1,
	weak_ref => 1,
	);

has 'scopedrels' => (
	is => 'ro',
	isa => 'HashRef[HashRef[Text::Tradition::Collation::Relationship]]',
	default => sub { {} },
	);

has 'graph' => (
	is => 'ro',
	isa => 'Graph',
	default => sub { Graph->new( undirected => 1 ) },
    handles => {
    	relationships => 'edges',
    	add_reading => 'add_vertex',
    	delete_reading => 'delete_vertex',
    },
	);
	
=head2 equivalence_graph()

Returns an equivalence graph of the collation, in which all readings
related via a 'colocated' relationship are transformed into a single
vertex. Can be used to determine the validity of a new relationship. 

=cut

has 'equivalence_graph' => (
	is => 'ro',
	isa => 'Graph',
	default => sub { Graph->new() },
	writer => '_reset_equivalence',
	);
	
has '_node_equivalences' => (
	is => 'ro',
	traits => ['Hash'],
	handles => {
		equivalence => 'get',
		set_equivalence => 'set',
		remove_equivalence => 'delete',
		_clear_equivalence => 'clear',
	},
	);

has '_equivalence_readings' => (
	is => 'ro',
	traits => ['Hash'],
	handles => {
		eqreadings => 'get',
		set_eqreadings => 'set',
		remove_eqreadings => 'delete',
		_clear_eqreadings => 'clear',
	},
	);
	
around add_reading => sub {
	my $orig = shift;
	my $self = shift;
	
	$self->equivalence_graph->add_vertex( @_ );
	$self->set_equivalence( $_[0], $_[0] );
	$self->set_eqreadings( $_[0], [ $_[0] ] );
	$self->$orig( @_ );
};

around delete_reading => sub {
	my $orig = shift;
	my $self = shift;
	
	$self->_remove_equivalence_node( @_ );
	$self->$orig( @_ );
};

=head2 get_relationship

Return the relationship object, if any, that exists between two readings.

=cut

sub get_relationship {
	my $self = shift;
	my @vector;
	if( @_ == 1 && ref( $_[0] ) eq 'ARRAY' ) {
		# Dereference the edge arrayref that was passed.
		my $edge = shift;
		@vector = @$edge;
	} else {
		@vector = @_;
	}
	my $relationship;
	if( $self->graph->has_edge_attribute( @vector, 'object' ) ) {
		$relationship = $self->graph->get_edge_attribute( @vector, 'object' );
	} 
	return $relationship;
}

sub _set_relationship {
	my( $self, $relationship, @vector ) = @_;
	$self->graph->add_edge( @vector );
	$self->graph->set_edge_attribute( @vector, 'object', $relationship );
	$self->_make_equivalence( @vector ) if $relationship->colocated;
}

=head2 create

Create a new relationship with the given options and return it.
Warn and return undef if the relationship cannot be created.

=cut

sub create {
	my( $self, $options ) = @_;
	# Check to see if a relationship exists between the two given readings
	my $source = delete $options->{'orig_a'};
	my $target = delete $options->{'orig_b'};
	my $rel = $self->get_relationship( $source, $target );
	if( $rel ) {
		if( $rel->type eq 'collated' ) {
			# Always replace a 'collated' relationship with a more descriptive
			# one, if asked.
			$self->del_relationship( $source, $target );
		} elsif( $rel->type ne $options->{'type'} ) {
			throw( "Another relationship of type " . $rel->type 
				. " already exists between $source and $target" );
		} else {
			return $rel;
		}
	}
	
	# Check to see if a nonlocal relationship is defined for the two readings
	$rel = $self->scoped_relationship( $options->{'reading_a'}, 
		$options->{'reading_b'} );
	if( $rel && $rel->type eq $options->{'type'} ) {
		return $rel;
	} elsif( $rel ) {
		throw( sprintf( "Relationship of type %s with scope %s already defined for readings %s and %s", $rel->type, $rel->scope, $options->{'reading_a'}, $options->{'reading_b'} ) );
	} else {
		$rel = Text::Tradition::Collation::Relationship->new( $options );
		$self->add_scoped_relationship( $rel ) if $rel->nonlocal;
		return $rel;
	}
}

=head2 add_scoped_relationship( $rel )

Keep track of relationships defined between specific readings that are scoped
non-locally.  Key on whichever reading occurs first alphabetically.

=cut

sub add_scoped_relationship {
	my( $self, $rel ) = @_;
	my $rdga = $rel->type eq 'orthographic' ? $rel->reading_a : lc( $rel->reading_a );
	my $rdgb = $rel->type eq 'orthographic' ? $rel->reading_b : lc( $rel->reading_b );	
	my $r = $self->scoped_relationship( $rdga, $rdgb );
	if( $r ) {
		warn sprintf( "Scoped relationship of type %s already exists between %s and %s",
			$r->type, $rdga, $rdgb );
		return;
	}
	my( $first, $second ) = sort ( $rdga, $rdgb );
	$self->scopedrels->{$first}->{$second} = $rel;
}

=head2 scoped_relationship( $reading_a, $reading_b )

Returns the general (document-level or global) relationship that has been defined 
between the two reading strings. Returns undef if there is no general relationship.

=cut

sub scoped_relationship {
	my( $self, $rdga, $rdgb ) = @_;
	my( $first, $second ) = sort( $rdga, $rdgb );
	if( exists $self->scopedrels->{$first}->{$second} ) {
		return $self->scopedrels->{$first}->{$second};
	} else {
		return undef;
	}
}

=head2 add_relationship( $self, $source, $sourcetext, $target, $targettext, $opts )

Adds the relationship specified in $opts (see Text::Tradition::Collation::Relationship 
for the possible options) between the readings given in $source and $target.  Sets
up a scoped relationship between $sourcetext and $targettext if the relationship is
scoped non-locally.

Returns a status boolean and a list of all reading pairs connected by the call to
add_relationship.

=begin testing

use Test::Warn;
use Text::Tradition;
use TryCatch;

my $t1;
warning_is {
	$t1 = Text::Tradition->new( 'input' => 'Self', 'file' => 't/data/legendfrag.xml' );
} 'DROPPING r14.2 -> r8.1: Cannot set relationship on a meta reading',
	"Got expected relationship drop warning on parse";

# Test 1.1: try to equate nodes that are prevented with an intermediate collation
ok( $t1, "Parsed test fragment file" );
my $c1 = $t1->collation;
my $trel = $c1->get_relationship( 'r9.2', 'r9.3' );
is( ref( $trel ), 'Text::Tradition::Collation::Relationship',
	"Troublesome relationship exists" );
is( $trel->type, 'collated', "Troublesome relationship is a collation" );

# Try to make the link we want
try {
	$c1->add_relationship( 'r8.6', 'r10.3', { 'type' => 'orthographic' } );
	ok( 1, "Added cross-collation relationship as expected" );
} catch( Text::Tradition::Error $e ) {
	ok( 0, "Existing collation blocked equivalence relationship: " . $e->message );
}

try {
	$c1->calculate_ranks();
	ok( 1, "Successfully calculated ranks" );
} catch ( Text::Tradition::Error $e ) {
	ok( 0, "Collation now has a cycle: " . $e->message );
}

# Test 1.2: attempt merge of an identical reading
try {
	$c1->merge_readings( 'r9.3', 'r11.5' );
	ok( 1, "Successfully merged reading 'pontifex'" );
} catch ( Text::Tradition::Error $e ) {
	ok( 0, "Merge of mergeable readings failed: $e->message" );
	
}

# Test 1.3: attempt relationship with a meta reading (should fail)
try {
	$c1->add_relationship( 'r8.1', 'r9.2', { 'type' => 'collated' } );
	ok( 0, "Allowed a meta-reading to be used in a relationship" );
} catch ( Text::Tradition::Error $e ) {
	is( $e->message, 'Cannot set relationship on a meta reading', 
		"Relationship link prevented for a meta reading" );
}

# Test 1.4: try to break a relationship near a meta reading
$c1->add_relationship( 'r7.6', 'r7.3', { type => 'orthographic' } );
try {
	$c1->del_relationship( 'r7.6', 'r7.7' );
	$c1->del_relationship( 'r7.6', 'r7.3' );
	ok( 1, "Relationship broken with a meta reading as neighbor" );
} catch {
	ok( 0, "Relationship deletion failed with a meta reading as neighbor" );
}

# Test 2.1: try to equate nodes that are prevented with a real intermediate
# equivalence
my $t2;
warning_is {
	$t2 = Text::Tradition->new( 'input' => 'Self', 'file' => 't/data/legendfrag.xml' );
} 'DROPPING r14.2 -> r8.1: Cannot set relationship on a meta reading',
	"Got expected relationship drop warning on parse";
my $c2 = $t2->collation;
$c2->add_relationship( 'r9.2', 'r9.3', { 'type' => 'lexical' } );
my $trel2 = $c2->get_relationship( 'r9.2', 'r9.3' );
is( ref( $trel2 ), 'Text::Tradition::Collation::Relationship',
	"Created blocking relationship" );
is( $trel2->type, 'lexical', "Blocking relationship is not a collation" );
# This time the link ought to fail
try {
	$c2->add_relationship( 'r8.6', 'r10.3', { 'type' => 'orthographic' } );
	ok( 0, "Added cross-equivalent bad relationship" );
} catch ( Text::Tradition::Error $e ) {
	like( $e->message, qr/witness loop/,
		"Existing equivalence blocked crossing relationship" );
}

try {
	$c2->calculate_ranks();
	ok( 1, "Successfully calculated ranks" );
} catch ( Text::Tradition::Error $e ) {
	ok( 0, "Collation now has a cycle: " . $e->message );
}

# Test 3.1: make a straightforward pair of transpositions.
my $t3 = Text::Tradition->new( 'input' => 'Self', 'file' => 't/data/lf2.xml' );
# Test 1: try to equate nodes that are prevented with an intermediate collation
my $c3 = $t3->collation;
try {
	$c3->add_relationship( 'r36.4', 'r38.3', { 'type' => 'transposition' } );
	ok( 1, "Added straightforward transposition" );
} catch ( Text::Tradition::Error $e ) {
	ok( 0, "Failed to add normal transposition: " . $e->message );
}
try {
	$c3->add_relationship( 'r36.3', 'r38.2', { 'type' => 'transposition' } );
	ok( 1, "Added straightforward transposition complement" );
} catch ( Text::Tradition::Error $e ) {
	ok( 0, "Failed to add normal transposition complement: " . $e->message );
}

# Test 3.2: try to make a transposition that could be a parallel.
try {
	$c3->add_relationship( 'r28.2', 'r29.2', { 'type' => 'transposition' } );
	ok( 0, "Added bad colocated transposition" );
} catch ( Text::Tradition::Error $e ) {
	like( $e->message, qr/Readings appear to be colocated/,
		"Prevented bad colocated transposition" );
}

# Test 3.3: make the parallel, and then make the transposition again.
try {
	$c3->add_relationship( 'r28.3', 'r29.3', { 'type' => 'orthographic' } );
	ok( 1, "Equated identical readings for transposition" );
} catch ( Text::Tradition::Error $e ) {
	ok( 0, "Failed to equate identical readings: " . $e->message );
}
try {
	$c3->add_relationship( 'r28.2', 'r29.2', { 'type' => 'transposition' } );
	ok( 1, "Added straightforward transposition complement" );
} catch ( Text::Tradition::Error $e ) {
	ok( 0, "Failed to add normal transposition complement: " . $e->message );
}

=end testing

=cut

sub add_relationship {
	my( $self, $source, $target, $options ) = @_;
    my $c = $self->collation;
	my $sourceobj = $c->reading( $source );
	my $targetobj = $c->reading( $target );
	throw( "Adding self relationship at $source" ) if $source eq $target;
	throw( "Cannot set relationship on a meta reading" )
		if( $sourceobj->is_meta || $targetobj->is_meta );
	my $relationship;
	my $thispaironly;
	my $droppedcolls = [];
	if( ref( $options ) eq 'Text::Tradition::Collation::Relationship' ) {
		$relationship = $options;
		$thispaironly = 1;  # If existing rel, set only where asked.
	} else {
		# Check the options
		$options->{'scope'} = 'local' unless $options->{'scope'};
		$options->{'scope'} = 'local' if $options->{'type'} eq 'collated';
		$options->{'scope'} = 'local' if $options->{'type'} eq 'transposition';
		
		my( $is_valid, $reason ) = $self->relationship_valid( $source, $target, 
			$options->{'type'}, $droppedcolls );
		unless( $is_valid ) {
			throw( "Invalid relationship: $reason" );
		}
		
		# Try to create the relationship object.
		$options->{'reading_a'} = $sourceobj->text;
		$options->{'reading_b'} = $targetobj->text;
		$options->{'orig_a'} = $source;
		$options->{'orig_b'} = $target;
    	if( $options->{'scope'} ne 'local' ) {
			# Is there a relationship with this a & b already?
			# Case-insensitive for non-orthographics.
			my $rdga = $options->{'type'} eq 'orthographic' 
				? $options->{'reading_a'} : lc( $options->{'reading_a'} );
			my $rdgb = $options->{'type'} eq 'orthographic' 
				? $options->{'reading_b'} : lc( $options->{'reading_b'} );
			my $otherrel = $self->scoped_relationship( $rdga, $rdgb );
			if( $otherrel && $otherrel->type eq $options->{type}
				&& $otherrel->scope eq $options->{scope} ) {
				warn "Applying existing scoped relationship";
				$relationship = $otherrel;
			}
    	}
		$relationship = $self->create( $options ) unless $relationship;  # Will throw on error
    }


	# Find all the pairs for which we need to set the relationship.
	my @vectors;
    if( $relationship->colocated && $relationship->nonlocal && !$thispaironly ) {
    	push( @vectors, $self->_find_applicable( $relationship ) );
    }
        
    # Now set the relationship(s).
    my @pairs_set;
	my $rel = $self->get_relationship( $source, $target );
	my $skip;
	if( $rel && $rel ne $relationship ) {
		if( $rel->nonlocal ) {
			throw( "Found conflicting relationship at $source - $target" );
		} elsif( $rel->type ne 'collated' ) {
			# Replace a collation relationship; leave any other sort in place.
			my $r1ann = $rel->has_annotation ? $rel->annotation : '';
			my $r2ann = $relationship->has_annotation ? $relationship->annotation : '';
			unless( $rel->type eq $relationship->type && $r1ann eq $r2ann ) {
				warn sprintf( "Not overriding local relationship %s with global %s " 
					. "set at %s -> %s (%s -> %s)", $rel->type, $relationship->type,
					$source, $target, $rel->reading_a, $rel->reading_b );
				$skip = 1;
			}
		}
	}
	$self->_set_relationship( $relationship, $source, $target ) unless $skip;
	push( @pairs_set, [ $source, $target ] );
    
    # Set any additional relationships that might be in @vectors.
    foreach my $v ( @vectors ) {
    	next if $v->[0] eq $source && $v->[1] eq $target;
    	next if $v->[1] eq $source && $v->[0] eq $target;
    	my @added = $self->add_relationship( @$v, $relationship );
    	push( @pairs_set, @added );
    }
    
    # Finally, restore whatever collations we can, and return.
    $self->_restore_collations( @$droppedcolls );
    return @pairs_set;
}

=head2 del_scoped_relationship( $reading_a, $reading_b )

Returns the general (document-level or global) relationship that has been defined 
between the two reading strings. Returns undef if there is no general relationship.

=cut

sub del_scoped_relationship {
	my( $self, $rdga, $rdgb ) = @_;
	my( $first, $second ) = sort( $rdga, $rdgb );
	return delete $self->scopedrels->{$first}->{$second};
}

sub _find_applicable {
	my( $self, $rel ) = @_;
	my $c = $self->collation;
	# TODO Someday we might use a case sensitive language.
	my $lang = $c->tradition->language;
	my @vectors;
	my @identical_readings;
	if( $rel->type eq 'orthographic' ) {
		@identical_readings = grep { $_->text eq $rel->reading_a } 
			$c->readings;
	} else {
		@identical_readings = grep { lc( $_->text ) eq lc( $rel->reading_a ) }
			$c->readings;
	}
	foreach my $ir ( @identical_readings ) {
		my @itarget;
		if( $rel->type eq 'orthographic' ) {
			@itarget = grep { $_->rank == $ir->rank 
							  && $_->text eq $rel->reading_b } $c->readings;
		} else {
			@itarget = grep { $_->rank == $ir->rank 
							  && lc( $_->text ) eq lc( $rel->reading_b ) } $c->readings;
		}
		if( @itarget ) {
			# Warn if there is more than one hit with no orth link between them.
			my $itmain = shift @itarget;
			if( @itarget ) {
				my %all_targets;
				map { $all_targets{$_} = 1 } @itarget;
				map { delete $all_targets{$_} } 
					$self->related_readings( $itmain, 
						sub { $_[0]->type eq 'orthographic' } );
    			warn "More than one unrelated reading with text " . $itmain->text
    				. " at rank " . $ir->rank . "!" if keys %all_targets;
			}
			push( @vectors, [ $ir->id, $itmain->id ] );
		}
	}
	return @vectors;
}

=head2 del_relationship( $source, $target )

Removes the relationship between the given readings. If the relationship is
non-local, removes the relationship everywhere in the graph.

=cut

sub del_relationship {
	my( $self, $source, $target ) = @_;
	my $rel = $self->get_relationship( $source, $target );
	return () unless $rel; # Nothing to delete; return an empty set.
	my $colo = $rel->colocated;
	my @vectors = ( [ $source, $target ] );
	$self->_remove_relationship( $colo, $source, $target );
	if( $rel->nonlocal ) {
		# Remove the relationship wherever it occurs.
		# Remove the relationship wherever it occurs.
		my @rel_edges = grep { $self->get_relationship( @$_ ) == $rel }
			$self->relationships;
		foreach my $re ( @rel_edges ) {
			$self->_remove_relationship( $colo, @$re );
			push( @vectors, $re );
		}
		$self->del_scoped_relationship( $rel->reading_a, $rel->reading_b );
	}
	return @vectors;
}

sub _remove_relationship {
	my( $self, $equiv, @vector ) = @_;
	$self->graph->delete_edge( @vector );
	$self->_break_equivalence( @vector ) if $equiv;
}
	
=head2 relationship_valid( $source, $target, $type )

Checks whether a relationship of type $type may exist between the readings given
in $source and $target.  Returns a tuple of ( status, message ) where status is
a yes/no boolean and, if the answer is no, message gives the reason why.

=cut

sub relationship_valid {
    my( $self, $source, $target, $rel, $mustdrop ) = @_;
    $mustdrop = [] unless $mustdrop; # in case we were passed nothing
    my $c = $self->collation;
    ## Assume validity is okay if we are initializing from scratch.
    return ( 1, "initializing" ) unless $c->tradition->_initialized;
        if ( $rel eq 'transposition' || $rel eq 'repetition' ) {
		# Check that the two readings do (for a repetition) or do not (for
		# a transposition) appear in the same witness.
		# TODO this might be called before witness paths are set...
		my %seen_wits;
		map { $seen_wits{$_} = 1 } $c->reading_witnesses( $source );
		foreach my $w ( $c->reading_witnesses( $target ) ) {
			if( $seen_wits{$w} ) {
				return ( 0, "Readings both occur in witness $w" ) 
					if $rel eq 'transposition';
				return ( 1, "ok" ) if $rel eq 'repetition';
			}
		}
		return ( 0, "Readings occur only in distinct witnesses" )
			if $rel eq 'repetition';
	} 
	if ( $rel eq 'transposition' ) {
		# We also need to check both that the readings occur in distinct
		# witnesses, and that they are not in the same place. That is,
		# proposing to link them should cause a witness loop.
		if( $self->test_equivalence( $source, $target ) ) {
			return ( 0, "Readings appear to be colocated, not transposed" );
		} else {
			return ( 1, "ok" );
		}
		
	} elsif( $rel ne 'repetition' ) {
		# Check that linking the source and target in a relationship won't lead
		# to a path loop for any witness. 
		# First, drop/stash any collations that might interfere
		my $sourceobj = $c->reading( $source );
		my $targetobj = $c->reading( $target );
		my $sourcerank = $sourceobj->has_rank ? $sourceobj->rank : -1;
		my $targetrank = $targetobj->has_rank ? $targetobj->rank : -1;
		unless( $rel eq 'collated' || $sourcerank == $targetrank ) {
			push( @$mustdrop, $self->_drop_collations( $source ) );
			push( @$mustdrop, $self->_drop_collations( $target ) );
			if( $c->end->has_rank ) {
				foreach my $rk ( $sourcerank .. $targetrank ) {
					map { push( @$mustdrop, $self->_drop_collations( $_->id ) ) }
						$c->readings_at_rank( $rk );
				}
			}
		}
		unless( $self->test_equivalence( $source, $target ) ) {
			$self->_restore_collations( @$mustdrop );
			return( 0, "Relationship would create witness loop" );
		}
		return ( 1, "ok" );
	}
}

sub _drop_collations {
	my( $self, $reading ) = @_;
	my @dropped;
	foreach my $n ( $self->graph->neighbors( $reading ) ) {
		if( $self->get_relationship( $reading, $n )->type eq 'collated' ) {
			push( @dropped, [ $reading, $n ] );
			$self->del_relationship( $reading, $n );
			#print STDERR "Dropped collation $reading -> $n\n";
		}
	}
	return @dropped;
}

sub _restore_collations {
	my( $self, @vectors ) = @_;
	foreach my $v ( @vectors ) {
		try {
			$self->add_relationship( @$v, { 'type' => 'collated' } );
			#print STDERR "Restored collation @$v\n";
		} catch {
			print STDERR $v->[0] . " - " . $v->[1] . " no longer collate\n";
		}
	}
}

=head2 filter_collations()

Utility function. Removes any redundant 'collated' relationships from the graph.
A collated relationship is redundant if the readings in question would occupy
the same rank regardless of the existence of the relationship.

=cut

sub filter_collations {
	my $self = shift;
	my $c = $self->collation;
	foreach my $r ( 1 .. $c->end->rank - 1 ) {
		my $anchor;
		my @need_collations;
		foreach my $rdg ( $c->readings_at_rank( $r ) ) {
			next if $rdg->is_meta;
			my $ip = 0;
			foreach my $pred ( $rdg->predecessors ) {
				if( $pred->rank == $r - 1 ) {
					$ip = 1;
					$anchor = $rdg unless( $anchor );
					last;
				}
			}
			push( @need_collations, $rdg ) unless $ip;
			$c->relations->_drop_collations( "$rdg" );
		}
		$anchor
			? map { $c->add_relationship( $anchor, $_, { 'type' => 'collated' } )
						unless $c->get_relationship( $anchor, $_ ) } @need_collations
			: warn "No anchor found at $r";
	}
}

=head2 related_readings( $reading, $filter )

Returns a list of readings that are connected via relationship links to $reading.
If $filter is set to a subroutine ref, returns only those related readings where
$filter( $relationship ) returns a true value.

=cut

sub related_readings {
	my( $self, $reading, $filter ) = @_;
	my $return_object;
	if( ref( $reading ) eq 'Text::Tradition::Collation::Reading' ) {
		$reading = $reading->id;
		$return_object = 1;
	}
	my @answer;
	if( $filter ) {
		# Backwards compat
		if( $filter eq 'colocated' ) {
			$filter = sub { $_[0]->colocated };
		} elsif( !ref( $filter ) ) {
			my $type = $filter;
			$filter = sub { $_[0]->type eq $type };
		}
		my %found = ( $reading => 1 );
		my $check = [ $reading ];
		my $iter = 0;
		while( @$check ) {
			my $more = [];
			foreach my $r ( @$check ) {
				foreach my $nr ( $self->graph->neighbors( $r ) ) {
					if( &$filter( $self->get_relationship( $r, $nr ) ) ) {
						push( @$more, $nr ) unless exists $found{$nr};
						$found{$nr} = 1;
					}
				}
			}
			$check = $more;
		}
		delete $found{$reading};
		@answer = keys %found;
	} else {
		@answer = $self->graph->all_reachable( $reading );
	}
	if( $return_object ) {
		my $c = $self->collation;
		return map { $c->reading( $_ ) } @answer;
	} else {
		return @answer;
	}
}

=head2 merge_readings( $kept, $deleted );

Makes a best-effort merge of the relationship links between the given readings, and
stops tracking the to-be-deleted reading.

=cut

sub merge_readings {
	my( $self, $kept, $deleted, $combined ) = @_;
	foreach my $edge ( $self->graph->edges_at( $deleted ) ) {
		# Get the pair of kept / rel
		my @vector = ( $kept );
		push( @vector, $edge->[0] eq $deleted ? $edge->[1] : $edge->[0] );
		next if $vector[0] eq $vector[1]; # Don't add a self loop
		
		# If kept changes its text, drop the relationship.
		next if $combined;
			
		# If kept / rel already has a relationship, just keep the old
		my $rel = $self->get_relationship( @vector );
		next if $rel;
		
		# Otherwise, adopt the relationship that would be deleted.
		$rel = $self->get_relationship( @$edge );
		$self->_set_relationship( $rel, @vector );
	}
	$self->_make_equivalence( $deleted, $kept );
}

### Equivalence logic

sub _remove_equivalence_node {
	my( $self, $node ) = @_;
	my $group = $self->equivalence( $node );
	my $nodelist = $self->eqreadings( $group );
	if( @$nodelist == 1 && $nodelist->[0] eq $node ) {
		$self->equivalence_graph->delete_vertex( $group );
		$self->remove_eqreadings( $group );
		$self->remove_equivalence( $group );
	} elsif( @$nodelist == 1 ) {
		throw( "DATA INCONSISTENCY in equivalence graph: " . $nodelist->[0] .
			" in group that should have only $node" );
	} else {
 		my @newlist = grep { $_ ne $node } @$nodelist;
		$self->set_eqreadings( $group, \@newlist );
		$self->remove_equivalence( $node );
	}
}

=head2 add_equivalence_edge

Add an edge in the equivalence graph corresponding to $source -> $target in the
collation. Should only be called by Collation.

=cut

sub add_equivalence_edge {
	my( $self, $source, $target ) = @_;
	my $seq = $self->equivalence( $source );
	my $teq = $self->equivalence( $target );
	$self->equivalence_graph->add_edge( $seq, $teq );
}

=head2 delete_equivalence_edge

Remove an edge in the equivalence graph corresponding to $source -> $target in the
collation. Should only be called by Collation.

=cut

sub delete_equivalence_edge {
	my( $self, $source, $target ) = @_;
	my $seq = $self->equivalence( $source );
	my $teq = $self->equivalence( $target );
	$self->equivalence_graph->delete_edge( $seq, $teq );
}

sub _is_disconnected {
	my $self = shift;
	return( scalar $self->equivalence_graph->predecessorless_vertices > 1
		|| scalar $self->equivalence_graph->successorless_vertices > 1 );
}

# Equate two readings in the equivalence graph
sub _make_equivalence {
	my( $self, $source, $target ) = @_;
	# Get the source equivalent readings
	my $seq = $self->equivalence( $source );
	my $teq = $self->equivalence( $target );
	# Nothing to do if they are already equivalent...
	return if $seq eq $teq;
	my $sourcepool = $self->eqreadings( $seq );
	# and add them to the target readings.
	push( @{$self->eqreadings( $teq )}, @$sourcepool );
	map { $self->set_equivalence( $_, $teq ) } @$sourcepool;
	# Then merge the nodes in the equivalence graph.
	foreach my $pred ( $self->equivalence_graph->predecessors( $seq ) ) {
		$self->equivalence_graph->add_edge( $pred, $teq );
	}
	foreach my $succ ( $self->equivalence_graph->successors( $seq ) ) {
		$self->equivalence_graph->add_edge( $teq, $succ );
	}
	$self->equivalence_graph->delete_vertex( $seq );
	# TODO enable this after collation parsing is done
	throw( "Graph got disconnected making $source / $target equivalence" )
		if $self->_is_disconnected && $self->collation->tradition->_initialized;
}

=head2 test_equivalence

Test whether, if two readings were equated with a 'colocated' relationship, 
the graph would still be valid.

=cut

sub test_equivalence {
	my( $self, $source, $target ) = @_;
	# Try merging the nodes in the equivalence graph; return a true value if
	# no cycle is introduced thereby. Restore the original graph first.
	
	# Keep track of edges we add
	my %added_pred;
	my %added_succ;
	# Get the reading equivalents
	my $seq = $self->equivalence( $source );
	my $teq = $self->equivalence( $target );
	# Maybe this is easy?
	return 1 if $seq eq $teq;
	
	# Save the first graph
	my $checkstr = $self->equivalence_graph->stringify();
	# Add and save relevant edges
	foreach my $pred ( $self->equivalence_graph->predecessors( $seq ) ) {
		if( $self->equivalence_graph->has_edge( $pred, $teq ) ) {
			$added_pred{$pred} = 0;
		} else {
			$self->equivalence_graph->add_edge( $pred, $teq );
			$added_pred{$pred} = 1;
		}
	}
	foreach my $succ ( $self->equivalence_graph->successors( $seq ) ) {
		if( $self->equivalence_graph->has_edge( $teq, $succ ) ) {
			$added_succ{$succ} = 0;
		} else {
			$self->equivalence_graph->add_edge( $teq, $succ );
			$added_succ{$succ} = 1;
		}
	}
	# Delete source equivalent and test
	$self->equivalence_graph->delete_vertex( $seq );
	my $ret = !$self->equivalence_graph->has_a_cycle;
	
	# Restore what we changed
	$self->equivalence_graph->add_vertex( $seq );
	foreach my $pred ( keys %added_pred ) {
		$self->equivalence_graph->add_edge( $pred, $seq );
		$self->equivalence_graph->delete_edge( $pred, $teq ) if $added_pred{$pred};
	}
	foreach my $succ ( keys %added_succ ) {
		$self->equivalence_graph->add_edge( $seq, $succ );
		$self->equivalence_graph->delete_edge( $teq, $succ ) if $added_succ{$succ};
	}
	unless( $self->equivalence_graph->eq( $checkstr ) ) {
		warn "GRAPH CHANGED after testing";
	}
	# Return our answer
	return $ret;
}

# Unmake an equivalence link between two readings. Should only be called internally.
sub _break_equivalence {
	my( $self, $source, $target ) = @_;
	
	# This is the hard one. Need to reconstruct the equivalence groups without
	# the given link.
	my( %sng, %tng );
	map { $sng{$_} = 1 } $self->_find_equiv_without( $source, $target );
	map { $tng{$_} = 1 } $self->_find_equiv_without( $target, $source );
	# If these groups intersect, they are still connected; do nothing.
	foreach my $el ( keys %tng ) {
		return if( exists $sng{$el} );
	}
	# If they don't intersect, then we split the nodes in the graph and in
	# the hashes. First figure out which group has which name
	my $oldgroup = $self->equivalence( $source ); # same as $target
	my $keepsource = $sng{$oldgroup};
	my $newgroup = $keepsource ? $target : $source;
	my( $oldmembers, $newmembers );
	if( $keepsource ) {
		$oldmembers = [ keys %sng ];
		$newmembers = [ keys %tng ];
	} else {
		$oldmembers = [ keys %tng ];
		$newmembers = [ keys %sng ];
	}
		
	# First alter the old group in the hash
	$self->set_eqreadings( $oldgroup, $oldmembers );
	foreach my $el ( @$oldmembers ) {
		$self->set_equivalence( $el, $oldgroup );
	}
	
	# then add the new group back to the hash with its new key
	$self->set_eqreadings( $newgroup, $newmembers );
	foreach my $el ( @$newmembers ) {
		$self->set_equivalence( $el, $newgroup );
	}
	
	# Now add the new group back to the equivalence graph
	$self->equivalence_graph->add_vertex( $newgroup );
	# ...add the appropriate edges to the source group vertext
	my $c = $self->collation;
	foreach my $rdg ( @$newmembers ) {
		foreach my $rp ( $c->sequence->predecessors( $rdg ) ) {
			next unless $self->equivalence( $rp );
			$self->equivalence_graph->add_edge( $self->equivalence( $rp ), $newgroup );
		}
		foreach my $rs ( $c->sequence->successors( $rdg ) ) {
			next unless $self->equivalence( $rs );
			$self->equivalence_graph->add_edge( $newgroup, $self->equivalence( $rs ) );
		}
	}
	
	# ...and figure out which edges on the old group vertex to delete.
	my( %old_pred, %old_succ );
	foreach my $rdg ( @$oldmembers ) {
		foreach my $rp ( $c->sequence->predecessors( $rdg ) ) {
			next unless $self->equivalence( $rp );
			$old_pred{$self->equivalence( $rp )} = 1;
		}
		foreach my $rs ( $c->sequence->successors( $rdg ) ) {
			next unless $self->equivalence( $rs );
			$old_succ{$self->equivalence( $rs )} = 1;
		}
	}
	foreach my $p ( $self->equivalence_graph->predecessors( $oldgroup ) ) {
		unless( $old_pred{$p} ) {
			$self->equivalence_graph->delete_edge( $p, $oldgroup );
		}
	}
	foreach my $s ( $self->equivalence_graph->successors( $oldgroup ) ) {
		unless( $old_succ{$s} ) {
			$self->equivalence_graph->delete_edge( $oldgroup, $s );
		}
	}
	# TODO enable this after collation parsing is done
	throw( "Graph got disconnected breaking $source / $target equivalence" )
		if $self->_is_disconnected && $self->collation->tradition->_initialized;
}

sub _find_equiv_without {
	my( $self, $first, $second ) = @_;
	my %found = ( $first => 1 );
	my $check = [ $first ];
	my $iter = 0;
	while( @$check ) {
		my $more = [];
		foreach my $r ( @$check ) {
			foreach my $nr ( $self->graph->neighbors( $r ) ) {
				next if $r eq $second;
				if( $self->get_relationship( $r, $nr )->colocated ) {
					push( @$more, $nr ) unless exists $found{$nr};
					$found{$nr} = 1;
				}
			}
		}
		$check = $more;
	}
	return keys %found;
}

=head2 rebuild_equivalence

(Re)build the equivalence graph from scratch. Dumps the graph, makes a new one,
adds all readings and edges, then makes an equivalence for all relationships.

=cut

sub rebuild_equivalence {
	my $self = shift;
	my $newgraph = Graph->new();
	# Set this as the new equivalence graph
	$self->_reset_equivalence( $newgraph );
	# Clear out the data hashes
	$self->_clear_equivalence;
	$self->_clear_eqreadings;
	
	# Add the readings
	foreach my $r ( $self->collation->readings ) {
		my $rid = $r->id;
		$newgraph->add_vertex( $rid );
		$self->set_equivalence( $rid, $rid );
		$self->set_eqreadings( $rid, [ $rid ] );
	}

	# Now add the edges
	foreach my $e ( $self->collation->paths ) {
		$self->add_equivalence_edge( @$e );
	}

	# Now equate the colocated readings. This does no testing; 
	# it assumes that all preexisting relationships are valid.
	foreach my $rel ( $self->relationships ) {
		my $relobj = $self->get_relationship( $rel );
		next unless $relobj && $relobj->colocated;
		$self->_make_equivalence( @$rel );
	}
}

=head2 equivalence_ranks 

Rank all vertices in the equivalence graph, and return a hash reference with
vertex => rank mapping.

=cut

sub equivalence_ranks {
	my $self = shift;
	my $eqstart = $self->equivalence( $self->collation->start );
	my $eqranks = { $eqstart => 0 };
	my $rankeqs = { 0 => [ $eqstart ] };
	my @curr_origin = ( $eqstart );
    # A little iterative function.
    while( @curr_origin ) {
        @curr_origin = $self->_assign_rank( $eqranks, $rankeqs, @curr_origin );
    }
	return( $eqranks, $rankeqs );
}

sub _assign_rank {
    my( $self, $node_ranks, $rank_nodes, @current_nodes ) = @_;
    my $graph = $self->equivalence_graph;
    # Look at each of the children of @current_nodes.  If all the child's 
    # parents have a rank, assign it the highest rank + 1 and add it to 
    # @next_nodes.  Otherwise skip it; we will return when the highest-ranked
    # parent gets a rank.
    my @next_nodes;
    foreach my $c ( @current_nodes ) {
        warn "Current reading $c has no rank!"
            unless exists $node_ranks->{$c};
        foreach my $child ( $graph->successors( $c ) ) {
            next if exists $node_ranks->{$child};
            my $highest_rank = -1;
            my $skip = 0;
            foreach my $parent ( $graph->predecessors( $child ) ) {
                if( exists $node_ranks->{$parent} ) {
                    $highest_rank = $node_ranks->{$parent} 
                        if $highest_rank <= $node_ranks->{$parent};
                } else {
                    $skip = 1;
                    last;
                }
            }
            next if $skip;
            my $c_rank = $highest_rank + 1;
            # print STDERR "Assigning rank $c_rank to node $child \n";
            $node_ranks->{$child} = $c_rank if $node_ranks;
            push( @{$rank_nodes->{$c_rank}}, $child ) if $rank_nodes;
            push( @next_nodes, $child );
        }
    }
    return @next_nodes;
}

### Output logic

sub _as_graphml { 
	my( $self, $graphml_ns, $xmlroot, $node_hash, $nodeid_key, $edge_keys ) = @_;
	
    my $rgraph = $xmlroot->addNewChild( $graphml_ns, 'graph' );
	$rgraph->setAttribute( 'edgedefault', 'directed' );
    $rgraph->setAttribute( 'id', 'relationships', );
    $rgraph->setAttribute( 'parse.edgeids', 'canonical' );
    $rgraph->setAttribute( 'parse.edges', 0 );
    $rgraph->setAttribute( 'parse.nodeids', 'canonical' );
    $rgraph->setAttribute( 'parse.nodes', 0 );
    $rgraph->setAttribute( 'parse.order', 'nodesfirst' );
    
    # Add the vertices according to their XML IDs
    my %rdg_lookup = ( reverse %$node_hash );
    # my @nlist = sort _by_xmlid keys( %rdg_lookup ); ## CAUSES SEGFAULT
    my @nlist = sort keys( %rdg_lookup );
    foreach my $n ( @nlist ) {
    	my $n_el = $rgraph->addNewChild( $graphml_ns, 'node' );
    	$n_el->setAttribute( 'id', $n );
    	_add_graphml_data( $n_el, $nodeid_key, $rdg_lookup{$n} );
    }
	$rgraph->setAttribute( 'parse.nodes', scalar @nlist );
    
    # Add the relationship edges, with their object information
    my $edge_ctr = 0;
    foreach my $e ( sort { $a->[0] cmp $b->[0] } $self->graph->edges ) {
    	# Add an edge and fill in its relationship info.
    	next unless( exists $node_hash->{$e->[0]} && exists $node_hash->{$e->[1]} );
		my $edge_el = $rgraph->addNewChild( $graphml_ns, 'edge' );
		$edge_el->setAttribute( 'source', $node_hash->{$e->[0]} );
		$edge_el->setAttribute( 'target', $node_hash->{$e->[1]} );
		$edge_el->setAttribute( 'id', 'e'.$edge_ctr++ );

		my $rel_obj = $self->get_relationship( @$e );
		foreach my $key ( keys %$edge_keys ) {
			my $value = $rel_obj->$key;
			_add_graphml_data( $edge_el, $edge_keys->{$key}, $value ) 
				if defined $value;
		}
	}
	$rgraph->setAttribute( 'parse.edges', $edge_ctr );
}

sub _by_xmlid {
	my $tmp_a = $a;
	my $tmp_b = $b;
	$tmp_a =~ s/\D//g;
	$tmp_b =~ s/\D//g;
	return $tmp_a <=> $tmp_b;
}

sub _add_graphml_data {
    my( $el, $key, $value ) = @_;
    return unless defined $value;
    my $data_el = $el->addNewChild( $el->namespaceURI, 'data' );
    $data_el->setAttribute( 'key', $key );
    $data_el->appendText( $value );
}

sub throw {
	Text::Tradition::Error->throw( 
		'ident' => 'Relationship error',
		'message' => $_[0],
		);
}

no Moose;
__PACKAGE__->meta->make_immutable;

1;
