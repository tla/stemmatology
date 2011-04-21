package Text::Tradition::Graph::Position;

use strict;
use warnings;

=head1 NAME

Text::Tradition::Graph::Position

=head1 SUMMARY

An object to go with a text graph that keeps track of relative
positions of the nodes on that graph.  This is useful for keeping
track of which readings are variants of each other, which is expensive
to calculate every time from the graph itself.

=head1 METHODS

=over 4

=item B<new>

Takes two arguments: a list of names of common nodes in the graph, and
a list of witness paths.  Calculates position identifiers for each
node based on this.

=cut

# TODO Why not just hand over the graph and calculate the common nodes
# and witness paths here?
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

=item B<node_position>

my $pos = $positions->node_position( $node );

Returns the position identifier for a given node in the graph.

=cut

sub node_position {
    my( $self, $node ) = @_;
    $node = _name( $node );

    unless( exists( $self->{'node_positions'}->{ $node } ) ) {
	warn "No node with name $node known to the graph";
	return;
    }

    return $self->{'node_positions'}->{ $node };
}

=item B<nodes_at_position>

my @nodes = $positions->nodes_at_position( $pos );

Returns the names of all the nodes in the graph at a given position.

=cut

sub nodes_at_position {
    my( $self, $pos ) = @_;

    my $positions = $self->_calc_positions();
    unless( exists $positions->{ $pos } ) {
	warn "No position $pos in the graph";
	return;
    }
    return @{ $positions->{ $pos }};
}

=item B<colocated_nodes>

my @nodes = $positions->colocated_nodes( $node );

Returns the names of all the nodes in the graph at the same position
as the node given, apart from that node itself.

=cut

sub colocated_nodes {
    my( $self, $node ) = @_;
    $node = _name( $node );
    my $pos = $self->node_position( $node );
    my @loc_nodes = $self->nodes_at_position( $pos );

    my @cn = grep { $_ !~ /^$node$/ } @loc_nodes;
    return @cn;
}

=item B<all>

my @position_list = $positions->all()

Returns an ordered list of positions in the graph.

=cut

sub all {
    my( $self ) = @_;
    my $pos = $self->_calc_positions;
    return sort by_position keys( %$pos );
}

sub witness_path {
    my( $self, $wit ) = @_;
    return @{$self->{'witness_paths'}->{ $wit }};
}

=back

=head2 Lemmatization functions

For some traditions, each position will have at least one node that is
the 'lemma text', that is, the text that an editor has chosen to stand
as authoritative for the tradition.  The following methods keep
track of what lemma, if any, should stand at each position.

=over

=item B<init_lemmatizer>

$positions->init_lemmatizer( @nodelist )

Sets up the necessary logic for keeping track of lemmas.  It should be
called once, with the initial list of lemmas.

=cut

# TODO We can initialize this without the argument, based on the passed
# list of common nodes.
sub init_lemmatizer {
    my( $self, @nodes ) = @_;
    foreach my $n ( @nodes ) {
	$self->set_state( $self->node_position( $n ), $n );
    }
}

=item B<state>

my $answer = $positions->state( $position_id )

For the given position ID, returns the node (if any) that stands at
the lemma.  If no node should stand as lemma at this position, returns
0; if no decision has been made for this position, returns undef.

=cut

sub state {
    my( $self, $pos ) = @_;
    return $self->{'position_state'}->{ $pos };
}

=item B<set_state>

$positions->set_state( $position_id, $state )

For the given position ID, sets the lemma (if any).  State can be the
name of a node, 0 (for cases when no lemma should stand), or undef
(for cases when no decision has been made).

=cut

sub set_state {
    my( $self, $pos, $state ) = @_;
    $self->{'position_state'}->{ $pos } = $state;
}

=back

=head2 Comparison function

=over

=item B<by_position>

my @nodelist = sort $positions->by_position @nodelist;

For use in the 'sort' function.  Returns a comparison value based on
the position of the given nodes.

=cut

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


#### HELPER FUNCTIONS ####

# At some point I may find myself using scalar references for the node
# positions, in order to keep them easily in sync.  Just in case, I will
# calculate this every time I need it.
sub _calc_positions {
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

=back

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews, aurum@cpan.org

=cut

1;
