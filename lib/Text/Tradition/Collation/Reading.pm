package Text::Tradition::Collation::Reading;

use Moose;
use MooseX::NonMoose;

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
		  default => [ $self ],
		  );

# This is a hash mapping of 'relationship => reading'.
# TODO we should validate the relationships sometime.
has 'equivalence' => (
		      is => 'ro',
		      isa => 'HashRef[Text::Tradition::Collation::Reading]',
		      default => {},
		      );

sub merge_from {
    my( $self, $merged_node ) = @_;
    # Adopt the identity pool of the other node.
    my @now_identical = grep { $_ ne $merged_node } @{$merged_node->same_as};
    my $new_pool = _merge_array_pool( \@now_identical, $self->same_as )
	if @now_identical;

    # Adopt the equivalence attributes of the other node.
    my $now_equiv = $merged_node->equivalence;
    foreach my $key ( %$now_equiv ) {
	if( $self->has_relationship( $key ) ) {
	    my $related = $self->get_relationship( $key );
	    if( $now_equiv->{$key} ne $related ) {
		warn( sprintf( "Merged reading %s has relationship %s to reading %s instead of %s; skipping",
			       $merged_node->name, $key,
			       $now_equiv->{$key},
			       $related) );
	    } # else no action needed
	} else {
	    $self->set_relationship( $key, $now_equiv->{$key} );
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
