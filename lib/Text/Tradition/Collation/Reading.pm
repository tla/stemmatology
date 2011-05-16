package Text::Tradition::Collation::Reading;

use Moose::Util::TypeConstraints;
use MooseX::NonMoose;
use Moose;

extends 'Graph::Easy::Node';

subtype 'Position'
    => as 'Str',
    => where { $_ =~ /^\d+\,\d+$/ },
    message { 'Position must be of the form x,y' };

has 'position' => (
    is => 'rw',
    isa => 'Position',
    );

# This contains an array of reading objects; the array is a pool,
# shared by the reading objects inside the pool.  When a reading is
# added to the pool, all the same_as attributes should be updated.
has 'same_as' => (
    is => 'rw',
    isa => 'ArrayRef[Text::Tradition::Collation::Reading]',
    );

# This is a hash mapping of 'relationship => reading'.
# TODO we should validate the relationships sometime.
has 'relationships' => (
    is => 'ro',
    isa => 'HashRef[Text::Tradition::Collation::Reading]',
    default => sub { {} },
    );

# Initialize the identity pool. 
sub BUILD {
    my( $self, $args ) = @_;
#    $self->same_as( [ $self ] );
}

sub merge_from {
    my( $self, $merged_node ) = @_;
    # Adopt the identity pool of the other node.
    my @now_identical = grep { $_ ne $merged_node } @{$merged_node->same_as};
    my $new_pool = _merge_array_pool( \@now_identical, $self->same_as )
	if @now_identical;

    # Adopt the relationship attributes of the other node.
    my $now_rel = $merged_node->relationships;
    foreach my $key ( %$now_rel ) {
	if( $self->has_relationship( $key ) ) {
	    my $related = $self->get_relationship( $key );
	    if( $now_rel->{$key} ne $related ) {
		warn( sprintf( "Merged reading %s has relationship %s to reading %s instead of %s; skipping",
			       $merged_node->name, $key,
			       $now_rel->{$key},
			       $related) );
	    } # else no action needed
	} else {
	    $self->set_relationship( $key, $now_rel->{$key} );
	}
    }
}

sub set_identical {
    my( $self, $other_node ) = @_; 
    my $enlarged_pool = _merge_array_pool( $self->same_as, 
					   $other_node->same_as );

    # ...and set this node to point to the enlarged pool.
    $self->set_same_as( $enlarged_pool );
}   

sub _merge_array_pool {
    my( $pool, $main_pool ) = @_;
    my %poolhash;
    foreach ( @$main_pool ) {
	# Note which nodes are already in the main pool so that we
	# don't re-add them.
	$poolhash{$_->name} = 1;
    }

    foreach( @$pool ) {
	# Add the remaining nodes to the main pool...
	push( @$main_pool, $_ ) unless $poolhash{$_->name};
    }
    return $main_pool;
}

# Much easier to do this with a hash than with an array of Relationship objects,
# which would be the proper OO method.

sub has_relationship {
    my( $self, $rel ) = @_;
    return exists( $self->relationships->{ $rel } );
}

sub get_relationship {
    my( $self, $rel ) = @_;
    if( $self->has_relationship( $rel ) ) {
	return $self->relationships->{ $rel };
    }
    return undef;
}

sub set_relationship {
    my( $self, $rel, $value ) = @_;
    $self->relationships->{ $rel } = $value;
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
