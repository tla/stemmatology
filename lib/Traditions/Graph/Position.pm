package Traditions::Graph::Position;

use strict;
use warnings;

=head1 NAME

Traditions::Graph::Position

=head1 SUMMARY

An object to go with a text graph that keeps track of relative
positions of the nodes.

=head1 METHODS

=over 4

=item B<new>

Takes two arguments: a list of names of common nodes in the graph, and
a list of witness paths.  Calculates position identifiers for each
node based on this.

=cut

sub new {
    my $proto = shift;
    my( $common_nodes, $witness_paths ) = @_;

    my $self = {};

    # We have to calculate the position identifiers for each word,
    # keyed on the common nodes.  This will be 'fun'.  The end result
    # is a hash per witness, whose key is the word node and whose
    # value is its position in the text.  Common nodes are always N,1
    # so have identical positions in each text.

    my $node_pos = {};
    foreach my $wit ( keys %$witness_paths ) {
	# First we walk each path, making a matrix for each witness that
	# corresponds to its eventual position identifier.  Common nodes
	# always start a new row, and are thus always in the first column.

	my $wit_matrix = [];
	my $cn = 0;  # We should hit the common nodes in order.
	my $row = [];
	foreach my $wn ( @{$witness_paths->{$wit}} ) { # $wn is a node name
	    if( $wn eq $common_nodes->[$cn] ) {
		# Set up to look for the next common node, and
		# start a new row of words.
		$cn++;
		push( @$wit_matrix, $row ) if scalar( @$row );
		$row = [];
	    }
	    push( @$row, $wn );
	}
	push( @$wit_matrix, $row );  # Push the last row onto the matrix

	# Now we have a matrix per witness, so that each row in the
	# matrix begins with a common node, and continues with all the
	# variant words that appear in the witness.  We turn this into
	# real positions in row,cell format.  But we need some
	# trickery in order to make sure that each node gets assigned
	# to only one position.

	foreach my $li ( 1..scalar(@$wit_matrix) ) {
	    foreach my $di ( 1..scalar(@{$wit_matrix->[$li-1]}) ) {
		my $node = $wit_matrix->[$li-1]->[$di-1];
		my $position = "$li,$di";
		# If we have seen this node before, we need to compare
		# its position with what went before.
		unless( exists $node_pos->{ $node } && 
			_cmp_position( $position, $node_pos->{ $node }) < 1 ) {
		    # The new position ID replaces the old one.
		    $node_pos->{$node} = $position;
		} # otherwise, the old position needs to stay.
	    }
	}
    }

    # Now we have a hash of node positions keyed on node.
    $self->{'node_positions'} = $node_pos;
    # We should also save our witness paths, as long as we have them.
    # Right now each path is a list of nodes; we may want to make it
    # a list of position refs.
    $self->{'witness_paths'} = $witness_paths;

    # We are also going to want to keep track of whether a position has
    # been explicitly emptied, for our lemmatization.
    my $position_state = {};
    map { $position_state->{ $_ } = undef } values %$node_pos;
    $self->{'position_state'} = $position_state;


    bless( $self, $proto );
    return $self;
}

sub node_position {
    my( $self, $node ) = @_;
    $node = _name( $node );

    unless( exists( $self->{'node_positions'}->{ $node } ) ) {
	warn "No node with name $node known to the graph";
	return;
    }

    return $self->{'node_positions'}->{ $node };
}

sub nodes_at_position {
    my( $self, $pos ) = @_;

    my $positions = $self->calc_positions();
    unless( exists $positions->{ $pos } ) {
	warn "No position $pos in the graph";
	return;
    }
    return @{ $positions->{ $pos }};
}

sub colocated_nodes {
    my( $self, $node ) = @_;
    $node = _name( $node );
    my $pos = $self->node_position( $node );
    my @loc_nodes = $self->nodes_at_position( $pos );

    my @cn = grep { $_ !~ /^$node$/ } @loc_nodes;
    return @cn;
}

# Returns an ordered list of positions in this graph
sub all {
    my( $self ) = @_;
    my $pos = $self->calc_positions;
    return sort by_position keys( %$pos );
}

# Returns undef if no decision has been taken on this position, the
# node name if there is a lemma for it, and 0 if there is no lemma for
# it.
sub state {
    my( $self, $pos ) = @_;
    return $self->{'position_state'}->{ $pos };
}

sub set_state {
    my( $self, $pos, $state ) = @_;
    $self->{'position_state'}->{ $pos } = $state;
}

sub init_lemmatizer {
    my( $self, @nodes ) = @_;
    foreach my $n ( @nodes ) {
	$self->set_state( $self->node_position( $n ), $n );
    }
}

sub witness_path {
    my( $self, $wit ) = @_;
    return @{$self->{'witness_paths'}->{ $wit }};
}

# At some point I may find myself using scalar references for the node
# positions, in order to keep them easily in sync.  Just in case, I will
# calculate this every time I need it.
sub calc_positions {
    my $self = shift;
    return _invert_hash( $self->{'node_positions'} )
}

# Helper for dealing with node refs
sub _name {
    my( $node ) = @_;
    # We work with node names in this library
    if( ref( $node ) && ref( $node ) eq 'Graph::Easy::Node' ) {
	$node = $node->name();
    }
    return $node;
}

### Comparison functions

# Compares two nodes according to their positions in the witness 
# index hash.
sub by_position {
    my $self = shift;
    return _cmp_position( $a, $b );
}

# Takes two position strings (X,Y) and sorts them.
sub _cmp_position {
    my( $a, $b ) = @_;
    my @pos_a = split(/,/, $a );
    my @pos_b = split(/,/, $b );

    my $big_cmp = $pos_a[0] <=> $pos_b[0];
    return $big_cmp if $big_cmp;
    # else 
    return $pos_a[1] <=> $pos_b[1];
}

# Useful helper.  Will be especially useful if I find myself using
# scalar references for the positions after all - it can dereference
# them here.
sub _invert_hash {
    my ( $hash, $plaintext_keys ) = @_;
    my %new_hash;
    foreach my $key ( keys %$hash ) {
        my $val = $hash->{$key};
        my $valkey = $val;
        if( $plaintext_keys 
            && ref( $val ) ) {
            $valkey = $plaintext_keys->{ scalar( $val ) };
            warn( "No plaintext value given for $val" ) unless $valkey;
        }
        if( exists ( $new_hash{$valkey} ) ) {
            push( @{$new_hash{$valkey}}, $key );
        } else {
            $new_hash{$valkey} = [ $key ];
        }
    }
    return \%new_hash;
}

1;
