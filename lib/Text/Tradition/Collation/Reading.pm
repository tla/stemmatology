package Text::Tradition::Collation::Reading;

use Moose;
use MooseX::NonMoose;
use KiokuDB::Set;
use KiokuDB::Util qw/ weak_set /;

extends 'Graph::Easy::Node';

has 'rank' => (
    is => 'rw',
    isa => 'Int',
    predicate => 'has_rank',
    );
    
has 'is_lacuna' => (
    is => 'rw',
    isa => 'Bool',
    );

# This contains an array of reading objects; the array is a pool,
# shared by the reading objects inside the pool.  When a reading is
# added to the pool, all the same_as attributes should be updated.
has 'same_as' => (
	does => 'KiokuDB::Set',
    is => 'rw',
    );
    
has 'is_primary' => (
	is => 'rw',
	isa => 'Bool',
	default => 1,
	);

# Deal with the non-arg option for Graph::Easy's constructor.
around BUILDARGS => sub {
	my $orig = shift;
	my $class = shift;

	my %args;
	if( @_ == 1 && ref( $_[0] ) ne 'HASH' ) {
		return $class->$orig( 'name' => $_[0] );
	} else {
		return $class->$orig( @_ );
	}
};

# A lacuna node is also a meta node.
before is_lacuna => sub {
	my( $self, $arg ) = @_;
	if( $arg ) {
		$self->is_meta( 1 );
	}
};

# Initialize the identity pool. 
sub BUILD {
	my( $self, $args ) = @_;
	my $pool = weak_set( $self );
	$self->same_as( $pool );
}

sub text {
    # Wrapper function around 'label' attribute.
    my $self = shift;
    if( @_ ) {
        if( defined $_[0] ) {
        	$self->set_attribute( 'label', $_[0] );
        } else {
            $self->del_attribute( 'label' );
        }
    }
    return $self->label;
}

sub witnessed_by {
	my( $self, $sigil, $backup ) = @_;
	my @wits = $self->witnesses;
	return 1 if grep { $_ eq $sigil } @wits;
	if( $backup ) {
		return 1 if grep { $_ eq $backup } @wits;
	}
	return 0;
}
    
sub witnesses {
	my( $self ) = @_;
	my @paths = grep { $_->get_attribute( 'class' ) eq 'path' } $self->outgoing;
	push( @paths, grep { $_->get_attribute( 'class' ) eq 'path' } $self->incoming );
	my %wits;
	foreach my $p ( @paths ) {
		if( $p->has_hidden_witnesses ) {
			foreach ( @{$p->hidden_witnesses} ) {
				$wits{$_} = 1;
			}
		} else {
			$wits{$p->label} = 1;
		}
	}
	return keys %wits;
}

sub merge_from {
	my( $self, $merged_node ) = @_;
	if( $self eq $merged_node ) {
		warn "Trying to merge a reading into itself";
		return;
	}
	# Adopt the identity pool of the other node.
	my @now_identical = grep { $_ ne $merged_node && $_ ne $self  }
		$merged_node->same_as->members;
	if( @now_identical ) {
		_merge_array_pool( $self->same_as, @now_identical );
	}
}

## Dealing with transposed readings.  These methods are only really
## applicable if we have a linear collation graph.

sub set_identical {
	my( $self, $other_node ) = @_;
	if( $self eq $other_node ) {
		warn "Trying to set a reading identical to itself";
		return;
	}
	# We are no longer primary; turn that off first.
	my @new_members = grep { $_ ne $other_node } $self->same_as->members;
	_merge_array_pool( $other_node->same_as, @new_members );
}	

sub identical_readings {
	my $self = shift;
	my @same = grep { $_ ne $self } $self->same_as->members;
	return @same;
}

## Helper function - 
sub _merge_array_pool {
	my( $pool, @new_members ) = @_;
	$pool->insert( @new_members );
	foreach my $n ( @new_members ) {
		$n->is_primary( 0 );
		$n->same_as( $pool );
	}
}

sub has_primary {
	my $self = shift;
	return !$self->is_primary;
}

sub primary {
	my $self = shift;
	my @p = grep { $_->is_primary } $self->same_as->members;
	warn "Identity pool for " . $self->name . " has more than one primary"
		if @p > 1;
	warn "Identity pool for " . $self->name . " has no primary" unless @p;
	return shift( @p );
}

# Looks from the outside like an accessor for a Boolean, but really 
# sets the node's class.  Should apply to start, end, and lacunae.

sub is_meta {
		my $self = shift;
		my $arg = shift;
		if( defined $arg && $arg ) {
				$self->set_attribute( 'class', 'meta' );
		} elsif ( defined $arg ) {
				$self->del_attribute( 'class' );
		}
		return $self->sub_class eq 'meta';		
}

# Returns all readings that adjoin this one on any path.
sub neighbor_readings {
	my( $self, $direction ) = @_;
	$direction = 'both' unless $direction;
	my @paths = grep { $_->isa( 'Text::Tradition::Collation::Path' ) } $self->edges;
	my %connected;
	foreach my $p ( @paths ) {
		if( $p->to eq $self ) {
			next if $direction eq 'forward';
			$connected{$p->from->name} = $p->from;
		} else { # $p->from eq $self
			next if $direction =~ /^back/;
			$connected{$p->to->name} = $p->to;
		}
	}
	return values( %connected );
}

# Returns all readings related to the one we've got.
sub related_readings {
	my( $self, $colocated, $queried ) = @_;
	$queried = { $self->name => 1 } unless $queried;
	my @related;
	# Get the nodes directly related to this one
	foreach my $e ( $self->edges ) {
		next unless $e->isa( 'Text::Tradition::Collation::Relationship' );
		next if $colocated && $e->type eq 'repetition';
		my $n = $e->from eq $self ? $e->to : $e->from;
		next if $queried->{$n->name};
		push( @related, $n );
	}
	# Now query those nodes for their relations, recursively
	map { $queried->{$_->name} = 1 } @related;
	my @also_related;
	foreach ( @related ) {
		push( @also_related, $_->related_readings( $colocated, $queried ) );
	}
	push( @related, @also_related );
	return @related;
}

## Keep track of which readings are unchanged across witnesses.
sub is_common {
	my( $self ) = shift;
	return $self->get_attribute( 'class' ) eq 'common';
}

## TODO Rationalize make_common, is_meta, etc.
sub make_common {
	my( $self ) = shift;
	$self->set_attribute( 'class', 'common' );
}

sub make_variant {
	my( $self ) = shift;
	$self->set_attribute( 'class', 'variant' );
}

no Moose;
__PACKAGE__->meta->make_immutable;

1;

######################################################
## copied from Graph::Easy::Parser docs
######################################################
# when overriding nodes, we also need ::Anon

package Text::Tradition::Collation::Reading::Anon;
use Moose;
use MooseX::NonMoose;
extends 'Text::Tradition::Collation::Reading';
extends 'Graph::Easy::Node::Anon';
no Moose;
__PACKAGE__->meta->make_immutable;

1;
# use base qw/Text::Tradition::Collation::Reading/;
# use base qw/Graph::Easy::Node::Anon/;

######################################################
# and :::Empty

package Text::Tradition::Collation::Reading::Empty;
use Moose;
use MooseX::NonMoose;
extends 'Graph::Easy::Node::Empty';
no Moose;
__PACKAGE__->meta->make_immutable;

1;
# use base qw/Text::Tradition::Collation::Reading/;

######################################################
