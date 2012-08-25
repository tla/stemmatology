package Text::Tradition::Analysis::Result;

use Moose;
use Digest::MD5 qw/ md5_hex /;
use Encode qw/ encode_utf8 /;
use JSON qw/ to_json /;
use Set::Scalar;
use Text::Tradition::Error;

=head1 NAME

Text::Tradition::Analysis::Result - object to express an IDP calculation result
for a particular graph problem.
    
=head1 DESCRIPTION

Given a graph (expressing a stemma hypothesis) and a set of witness groupings 
(expressing variation in reading between witnesses related according to the
stemma hypothesis), it is possible to calculate certain properties of how the
readings might be related to each other. This calculation depends on a custom
program run under the IDP system [TODO URL]. As the problem is NP-hard, the
calculation can take a long time. The purpose of this object is to allow storage
of calculated results in a database.

For each graph problem, the following features can be calculated:

=over 4

=item * Whether the reading groups form a genealogical pattern on the stemma.

=item * The groupings, including lost/hypothetical witnesses if necessary, that minimize the amount of non-genealogical variation on the stemma.

=item * The classes, which for each witness express whether (in a minimally non-genealogical case) the witness is a source of its reading, follows a parent witness, or reverts to an ancestral reading that is not the parent's.

=back

=head1 CONSTRUCTOR

=head2 new

Creates a new graph problem. Requires two properties:

=over 4

=item * setlist - An array of arrays expressing the witness sets. The inner
arrays will be converted to Set::Scalar objects, and must have distinct members.

=item * graph - A dot description of a graph (e.g. the output of a call to
Text::Tradition::Stemma::editable) against which the sets will be analyzed.

=back

=cut

has 'setlist' => (
	traits => ['Array'],
	isa => 'ArrayRef[Set::Scalar]',
	handles => {
		sets => 'elements',
		set_index => 'first_index',
	},
	required => 1
);

has 'graph' => (
	is => 'ro',
	isa => 'Str',
	required => 1
);

has 'status' => (
	is => 'rw',
	isa => 'Str'
);

has 'is_genealogical' => (
	is => 'rw',
	isa => 'Bool',
	predicate => 'has_genealogical_result'
);

has 'groupinglist' => (
	traits => ['Array'],
	isa => 'ArrayRef[Set::Scalar]',
	handles => {
		groupings => 'elements',
		_add_grouping => 'push',
		_set_grouping => 'set',
		grouping => 'get',
	},
	default => sub { [] }
);

has 'classlist' => (
	traits => ['Hash'],
	isa => 'HashRef[Str]',
	handles => {
		class => 'get',
		has_class => 'exists',
		set_class => 'set',
		classes => 'elements',
		assigned_wits => 'keys',
	},
);

around BUILDARGS => sub {
	my $orig = shift;
	my $class = shift;
	my $args = @_ == 1 ? $_[0] : { @_ };
	
	# Convert the set list into a list of Set::Scalars, ordered first by size and
	# then alphabetically by first-sorted.
	die "Must specify a set list to Analysis::Result->new()" 
		unless ref( $args->{'setlist'} ) eq 'ARRAY'; 
	# Order the sets and make sure they are all distinct Set::Scalars.
	$args->{'setlist'} = [ sort { by_size_and_alpha( $a, $b ) } 
							_check_set_args( $args->{'setlist'} ) ];
	$args->{'groupinglist'} = [ _check_set_args( $args->{'groupinglist'} ) ];
	
	# If we have been passed a Text::Tradition::Stemma or a Graph, save only
	# its string.
	if( ref( $args->{'graph'} ) ) {
		my $st = delete $args->{'graph'};
		my $type = ref( $st );
		my $gopt = { linesep => ' ' };
		if( $type eq 'Text::Tradition::Stemma' ) {
			$args->{'graph'} = $st->editable( $gopt );
		} elsif( $type eq 'Graph' ) {
			$args->{'graph'} = Text::Tradition::Stemma::editable_graph( $st, $gopt );
		} else {
			die "Passed argument to graph that is neither Stemma nor Graph";
		}
	} 
	
	# If our only args are graph and setlist, then status should be 'new'
	if( scalar keys %$args == 2 ) {
		$args->{'status'} = 'new';
	}
		
	return $class->$orig( $args );
};

sub _check_set_args {
	my $setlist = shift;
	my @sets;
	foreach my $set ( @{$setlist} ) {
		my $s = $set;
		# Check uniqueness of the current set
		if( ref( $set ) ne 'Set::Scalar' ) {
			$s = Set::Scalar->new( @$set );
			die "Duplicate element(s) in set or group passed to Analysis::Result->new()"
				unless @$set == $s->elements;
		}
		# Check distinctness of the set from all other sets given so far
		foreach my $ps ( @sets ) {
			die "Two sets are not disjoint"
				unless $s->is_disjoint( $ps );
		}
		# Save the set.
		push( @sets, $s );
	}
	return @sets;
}	

sub BUILD {
	my $self = shift;
	
	# Initialize the groupings array
	map { $self->_add_grouping( $_ ) } $self->sets;
}

sub record_grouping {
	my( $self, $group ) = @_;
	unless( ref( $group ) eq 'Set::Scalar' ) {
		my $s = Set::Scalar->new( @$group );
		$group = $s;
	}
	# Find the set that is a subset of this group, and record it in the
	# correct spot in our groupinglist.
	my $idx = 0;
	foreach my $set ( $self->sets ) {
		if( _is_subset( $set, $group ) ) {
			$self->_set_grouping( $idx, $group );
			last;
		}
		$idx++;
	}
	if( $idx == scalar( $self->sets ) ) {
		throw( "Failed to find witness set that is a subset of $group" );
	}
}

sub _is_subset {
    # A replacement for the stupid Set::Scalar::is_subset
    my( $set1, $set2 ) = @_;
    my %all;
    map { $all{$_} = 1 } $set2->members;
    foreach my $m ( $set1->members ) {
        return 0 unless $all{$m};
    }
    return 1;
}

# A request string is the graph followed by the groups, which should form a unique
# key for the result.
sub object_key {
	my $self = shift;
	return md5_hex( encode_utf8( $self->request_string ) );
}

sub request_string {
	my $self = shift;
	return string_from_graph_problem( $self->graph, [ $self->sets ] );
}

sub string_from_graph_problem {
	my( $graph, $grouping ) = @_;
	my( $graphstr, @groupsets );
	# Get the graph string
	if( ref( $graph ) && ref( $graph ) eq 'Graph' ) {
		$graphstr = Text::Tradition::Stemma::editable_graph( $graph, { 'linesep' => ' ' } );
	} else {
		throw( "Passed non-graph object $graph to stringification" )
			if ref( $graph );
		$graphstr = $graph;
	}
	# Make sure all groupings are sets
	foreach my $g ( @$grouping ) {
		if( ref( $g ) eq 'ARRAY' ) {
			push( @groupsets, Set::Scalar->new( @$g ) );
		} elsif( ref( $g ) eq 'Set::Scalar' ) {
			push( @groupsets, $g );
		} else {
			throw( "Tried to stringify grouping $g that is neither set nor array" );
		}
	}
	return $graphstr . '//' . 
		join( ',', sort { by_size_and_alpha( $a, $b ) } @groupsets );
}

# This should work as $self->problem_json or as problem_json( @objects )
sub problem_json {
	my( @objects ) = @_;
	# There should be a distinct problem for each unique graph.
	my %distinct_problems;
	foreach my $o ( @objects ) {
		unless( exists $distinct_problems{$o->graph} ) {
			$distinct_problems{$o->graph} = [];
		}
		my @groupings;
		map { push( @groupings, [ $_->members ] ) } $o->sets;
		push( @{$distinct_problems{$o->graph}}, \@groupings );
	}
	my @pstrs = map { to_json( 
		{ graph => $_, groupings => $distinct_problems{$_} } ) } 
		keys %distinct_problems;
	return @pstrs;
}

sub by_size_and_alpha {
	my( $a, $b ) = @_;
	my $size = $b->members <=> $a->members;
	return $size if $size;
	# Then sort by alphabetical order of set elements.
	return "$a" cmp "$b";
}

sub sources {
	my $self = shift;
	my @sources = grep { $self->class( $_ ) eq 'source' } $self->assigned_wits;
	return @sources;
}

# Look for a matching set in our setlist, and return its corresponding group
sub minimum_grouping_for {
	my( $self, $set ) = @_;
	my $midx = $self->set_index( sub { "$set" eq "$_" } );
	return undef unless defined $midx;
	return $self->grouping( $midx );
}

sub TO_JSON {
	my $self = shift;
	# Graph and setlist
	my $data = { 
		graph => $self->graph, 
		setlist => [],
		groupinglist => [],
		classlist => {}
	};
	$data->{is_genealogical} = 1 if $self->is_genealogical;
	foreach my $set ( $self->sets ) {
		push( @{$data->{setlist}}, [ $set->members ] );
	}
	# groupinglist
	foreach my $group ( $self->groupings ) {
		push( @{$data->{groupinglist}}, [ $group->members ] );
	}
	# classlist
	foreach my $wit ( $self->assigned_wits ) {
		$data->{classlist}->{$wit} = $self->class( $wit );
	}
	return $data;
}

sub throw {
	Text::Tradition::Error->throw( 
		'ident' => 'Analysis::Result error',
		'message' => $_[0],
	);
}

no Moose;
__PACKAGE__->meta->make_immutable;

1;
