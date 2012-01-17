package Text::Tradition::Collation::RelationshipStore;

use strict;
use warnings;
use Text::Tradition::Collation::Relationship;

use Moose;

=head1 NAME

Text::Tradition::Collation::RelationshipStore - Keeps track of the relationships
between readings in a given collation
    
=head1 DESCRIPTION

Text::Tradition is a library for representation and analysis of collated
texts, particularly medieval ones.  The RelationshipStore is an internal object
of the collation, to keep track of the defined relationships (both specific and
general) between readings.

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
	
=head2 create

Create a new relationship with the given options and return it.
Warn and return undef if the relationship cannot be created.

=cut

sub create {
	my( $self, $options ) = @_;
	# Check to see if a relationship exists between the two given readings
	my $source = delete $options->{'orig_a'};
	my $target = delete $options->{'orig_b'};
	my $rel;
	if( $self->graph->has_edge( $source, $target ) ) {
		$rel = $self->graph->get_edge_attribute( $source, $target, 'object' );
		if( $rel->type ne $options->{'type'} ) {
			warn "Another relationship of type " . $rel->type 
				. " already exists between $source and $target";
			return;
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
		warn sprintf( "Relationship of type %s with scope %s already defined for readings %s and %s", $rel->type, $rel->scope, $options->{'reading_a'}, $options->{'reading_b'} );
		return;
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
	my $r = $self->scoped_relationship( $rel->reading_a, $rel->reading_b );
	if( $r ) {
		warn sprintf( "Scoped relationship of type %s already exists between %s and %s",
			$r->type, $rel->reading_a, $rel->reading_b );
		return;
	}
	$self->scopedrels->{$rel->reading_a}->{$rel->reading_b} = $rel;
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

=cut

sub add_relationship {
	my( $self, $source, $source_rdg, $target, $target_rdg, $options ) = @_;

	# Check the options
	$options->{'scope'} = 'local' unless $options->{'scope'};
	
	my( $is_valid, $reason ) = 
		$self->relationship_valid( $source, $target, $options->{'type'} );
    unless( $is_valid ) {
        return ( undef, $reason );
    }
    
    # Try to create the relationship object.
    $options->{'reading_a'} = $source_rdg->text;
    $options->{'reading_b'} = $target_rdg->text;
    $options->{'orig_a'} = $source;
    $options->{'orig_b'} = $target;
    my $relationship = $self->create( $options );
	return( undef, "Relationship creation failed" ) unless $relationship;

	# Find all the pairs for which we need to set the relationship.
	my @vectors = ( [ $source, $target ] );	
    if( $relationship->colocated && $relationship->nonlocal ) {
    	my $c = $self->collation;
    	# Set the same relationship everywhere we can, throughout the graph.
    	my @identical_readings = grep { $_->text eq $relationship->reading_a }
    		$c->readings;
    	foreach my $ir ( @identical_readings ) {
    		next if $ir->id eq $source;
    		# Check to see if there is a target reading with the same text at
    		# the same rank.
    		my @itarget = grep 
    			{ $_->rank == $ir->rank && $_->text eq $relationship->reading_b }
    			$c->readings;
    		if( @itarget ) {
    			# We found a hit.
    			warn "More than one reading with text " . $target_rdg->text
    				. " at rank " . $ir->rank . "!" if @itarget > 1;
    			push( @vectors, [ $ir->id, $itarget[0]->id ] );
    		}
    	}	
    }
    
    # Now set the relationship(s).
    my @pairs_set;
    foreach my $v ( @vectors ) {
    	if( $self->graph->has_edge( @$v ) ) {
    		# Is it locally scoped?
    		my $rel = $self->graph->get_edge_attribute( @$v, 'object' );
    		if( $rel->nonlocal ) {
    			# TODO I think we should not be able to get here.
    			warn "Found conflicting relationship at @$v";
    		} else {
    			warn "Not overriding local relationship set at @$v";
    			next;
    		}
    	}
    	$self->graph->add_edge( @$v );
    	$self->graph->set_edge_attribute( @$v, 'object', $relationship );
    	push( @pairs_set, $v );
    }
    
    return( 1, @pairs_set );
}

=head2 relationship_valid( $source, $target, $type )

Checks whether a relationship of type $type may exist between the readings given
in $source and $target.  Returns a tuple of ( status, message ) where status is
a yes/no boolean and, if the answer is no, message gives the reason why.

=cut

sub relationship_valid {
    my( $self, $source, $target, $rel ) = @_;
    my $c = $self->collation;
    if ( $rel eq 'transposition' || $rel eq 'repetition' ) {
		# Check that the two readings do (for a repetition) or do not (for
		# a transposition) appear in the same witness.
		my %seen_wits;
		map { $seen_wits{$_} = 1 } $c->reading_witnesses( $source );
		foreach my $w ( $c->reading_witnesses( $target ) ) {
			if( $seen_wits{$w} ) {
				return ( 0, "Readings both occur in witness $w" ) 
					if $rel eq 'transposition';
				return ( 1, "ok" ) if $rel eq 'repetition';
		}
		return $rel eq 'transposition' ? ( 1, "ok" )
			: ( 0, "Readings occur only in distinct witnesses" );
		}
	} else {
		# Check that linking the source and target in a relationship won't lead
		# to a path loop for any witness.  First make a lookup table of all the
		# readings related to either the source or the target.
		my @proposed_related = ( $source, $target );
		push( @proposed_related, $self->related_readings( $source, 'colocated' ) );
		push( @proposed_related, $self->related_readings( $target, 'colocated' ) );
		my %pr_ids;
		map { $pr_ids{ $_ } = 1 } @proposed_related;
	
		# None of these proposed related readings should have a neighbor that
		# is also in proposed_related.
		foreach my $pr ( keys %pr_ids ) {
			foreach my $neighbor( $c->sequence->neighbors( $pr ) ) {
				return( 0, "Would relate neighboring readings $pr and $neighbor" )
					if exists $pr_ids{$neighbor};
			}
		}		
		return ( 1, "ok" );
	}
}

=head2 related_readings( $reading, $colocated_only )

Returns a list of readings that are connected via relationship links to $reading.
If $colocated_only is true, restricts the list to those readings that are in the
same logical location (and therefore have the same rank in the collation graph.)

=cut

sub related_readings {
	my( $self, $reading, $colocated ) = @_;
	my $return_object;
	if( ref( $reading ) eq 'Text::Tradition::Collation::Reading' ) {
		$reading = $reading->id;
		$return_object = 1;
	}
	my @answer;
	if( $colocated ) {
		my %found = ( $reading => 1 );
		my $check = [ $reading ];
		my $iter = 0;
		while( @$check ) {
			my $more = [];
			foreach my $r ( @$check ) {
				foreach my $nr ( $self->graph->neighbors( $r ) ) {
					if( $self->graph->get_edge_attribute( $r, $nr, 'object' )->colocated ) {
						push( @$more, $nr ) unless exists $found{$nr};
						$found{$nr} = 1;
					}
				}
			}
			$check = $more;
		}
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
			
		# If kept / rel already has a relationship, warn and keep the old
		if( $self->graph->has_edge( @vector ) ) {
			warn sprintf( "Readings %s and %s have existing relationship; dropping link with %s", @vector, $deleted );
			next;
		}
		
		# Otherwise, adopt the relationship that would be deleted.
		my $rel = $self->graph->get_edge_attribute( @$edge, 'object' );
		$self->graph->add_edge( @vector );
		$self->graph->set_edge_attribute( @vector, 'object', $rel );
	}
	$self->delete_reading( $deleted );
}

sub as_graphml { 
	my( $self, $graphml_ns, $xmlroot, $node_hash, $nodeid_key, $edge_keys ) = @_;
	
    my $rgraph = $xmlroot->addNewChild( $graphml_ns, 'graph' );
	$rgraph->setAttribute( 'edgedefault', 'directed' );
    $rgraph->setAttribute( 'id', 'relationships', );
    $rgraph->setAttribute( 'parse.edgeids', 'canonical' );
    $rgraph->setAttribute( 'parse.edges', scalar($self->graph->edges) );
    $rgraph->setAttribute( 'parse.nodeids', 'canonical' );
    $rgraph->setAttribute( 'parse.nodes', scalar($self->graph->vertices) );
    $rgraph->setAttribute( 'parse.order', 'nodesfirst' );
    
    # Add the vertices according to their XML IDs
    my %rdg_lookup = ( reverse %$node_hash );
    foreach my $n ( sort _by_xmlid keys( %rdg_lookup ) ) {
    	my $n_el = $rgraph->addNewChild( $graphml_ns, 'node' );
    	$n_el->setAttribute( 'id', $n );
    	_add_graphml_data( $n_el, $nodeid_key, $rdg_lookup{$n} );
    }
    
    # Add the relationship edges, with their object information
    my $edge_ctr = 0;
    foreach my $e ( sort { $a->[0] cmp $b->[0] } $self->graph->edges ) {
    	# Add an edge and fill in its relationship info.
		my $edge_el = $rgraph->addNewChild( $graphml_ns, 'edge' );
		$edge_el->setAttribute( 'source', $node_hash->{$e->[0]} );
		$edge_el->setAttribute( 'target', $node_hash->{$e->[1]} );
		$edge_el->setAttribute( 'id', 'e'.$edge_ctr++ );

		my $rel_obj = $self->graph->get_edge_attribute( @$e, 'object' );
		_add_graphml_data( $edge_el, $edge_keys->{'relationship'}, $rel_obj->type );
		_add_graphml_data( $edge_el, $edge_keys->{'scope'}, $rel_obj->scope );
		_add_graphml_data( $edge_el, $edge_keys->{'non_correctable'}, 
			$rel_obj->non_correctable ) if $rel_obj->noncorr_set;
		_add_graphml_data( $edge_el, $edge_keys->{'non_independent'}, 
			$rel_obj->non_independent ) if $rel_obj->nonind_set;
	}
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

no Moose;
__PACKAGE__->meta->make_immutable;

1;
