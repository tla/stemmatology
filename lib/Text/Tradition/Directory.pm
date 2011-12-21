package Text::Tradition::Directory;

use strict;
use warnings;
use Moose;
use KiokuDB::TypeMap;
use KiokuDB::TypeMap::Entry::Naive;

extends 'KiokuX::Model';

has data_hash => (
    traits => ['Hash'],
	default => sub { {} },
    handles => {
        tradition     => 'get',
        stemma		  => 'get',
        add_tradition => 'set',
        add_stemma	  => 'set',
        traditions    => 'keys',
    },
);
	
has typemap => (
	is => 'rw',
	isa => 'KiokuDB::TypeMap',
	default => sub { 
		KiokuDB::TypeMap->new(
			isa_entries => {
				"Graph::Easy::Base" => KiokuDB::TypeMap::Entry::Naive->new,
				"Graph" => KiokuDB::TypeMap::Entry::Naive->new,
				"Graph::AdjacencyMap" => KiokuDB::TypeMap::Entry::Naive->new,
			}
		);
	},
);

around 'tradition' => sub {
	my( $orig, $self, @arg ) = @_;
	my $data = $self->$orig( @arg );
	return $data->{'object'};
};

around 'stemma' => sub {
	my( $orig, $self, @arg ) = @_;
	my $data = $self->$orig( @arg );
	return $data->{'stemma'};
};

around 'add_tradition' => sub {
	my( $orig, $self, $id, $obj ) = @_;
	$self->$orig( $id => { 'object' => $obj } );
};

around 'add_stemma' => sub {
	my( $orig, $self, $id, $obj ) = @_;
	$self->{data_hash}->{$id}->{'stemma'} = $obj;
};

# Load all the relevant data from the DSN we were passed.

sub BUILD {
	my $self = shift;
	my $args = shift;
	
	if( exists $args->{'dsn'} ) {
		# Connect to self, get the traditions and stemmas, and save them
		# in the directory.
		my $scope = $self->new_scope;
		my $stream = $self->root_set;
		my %stemmata;
		until( $stream->is_done ) {
			foreach my $obj ( $stream->items ) {
				my $uuid = $self->object_to_id( $obj );
				if( ref( $obj ) eq 'Text::Tradition' ) {
					$self->add_tradition( $uuid => $obj );
				} elsif( ref( $obj ) eq 'Text::Tradition::Stemma' ) {
					$stemmata{$obj->collation} = $obj;
				} else {
					warn "Found root object in DB that is neither tradition nor stemma: $obj";
				}
			}
		}
		# Now match the stemmata to their traditions.
		foreach my $id ( $self->traditions ) {
			my $c = $self->tradition( $id )->collation;
			if( exists $stemmata{$c} ) {
				$self->add_stemma( $id => $stemmata{$c} );
			}
		}
	}
}

1;
		
		