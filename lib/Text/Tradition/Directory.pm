package Text::Tradition::Directory;

use strict;
use warnings;
use Moose;
use KiokuDB::TypeMap;
use KiokuDB::TypeMap::Entry::Naive;

extends 'KiokuX::Model';

=head1 NAME

Text::Tradition::Directory - a KiokuDB interface for storing and retrieving traditions

=head1 SYNOPSIS

  use Text::Tradition::Directory;
  my $d = Text::Tradition::Directory->new( 
    'dsn' => 'dbi:SQLite:mytraditions.db',
    'extra_args' => { 'create' => 1 },
  );
  
  my $tradition = Text::Tradition->new( @args );
  $d->save_tradition( $tradition );
  my $stemma = Text::Tradition::Stemma->new( 
  	'dot' => $dotfile, 'collation' => $tradition->collation );
  $d->save_stemma( $stemma );
  
  foreach my $id ( $d->traditions ) {
  	print $d->tradition( $id )->name;
  	print $d->stemma( $id )->as_svg;
  }
    
=head1 DESCRIPTION

Text::Tradition::Directory is an interface for storing and retrieving text traditions and all their data, including an associated stemma hypothesis.  It is an instantiation of a KiokuDB::Model, storing traditions and associated stemmas by UUID.

=head1 METHODS

=head2 new

Returns a Directory object.  Apart from those documented in L<KiokuX::Model>,
options include:

=over

=item * preload - Load all traditions and stemmata into memory upon instantiation.  Defaults to true.  (TODO manage on-demand loading)

=back

=head2 tradition_ids

Returns the ID of all traditions in the database.

=head2 tradition( $id )

Returns the Text::Tradition object of the given ID.

=head2 stemma( $id )

Returns the Text::Tradition::Stemma object associated with the given tradition ID.

=head2 save_tradition( $tradition )

Writes the given tradition to the database, returning its UUID.

=head2 save_stemma( $stemma )

Writes the given stemma to the database, returning its UUID.

=begin testing

use File::Temp;
use Text::Tradition;
use Text::Tradition::Stemma;
use_ok 'Text::Tradition::Directory';

my $fh = File::Temp->new();
my $file = $fh->filename;
$fh->close;
my $dsn = "dbi:SQLite:dbname=$file";

my $d = Text::Tradition::Directory->new( 'dsn' => $dsn,
    'extra_args' => { 'create' => 1 } );
is( ref $d, 'Text::Tradition::Directory', "Got directory object" );

my $t = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'Tabular',
    'file'  => 't/data/simple.txt',
    );
my $uuid = $d->save_tradition( $t );
ok( $uuid, "Saved test tradition" );

my $s = Text::Tradition::Stemma->new( 
	'collation' => $t->collation,
	'dotfile' => 't/data/simple.dot' );
my $sid = $d->save_stemma( $s );
ok( $sid, "Saved test stemma" );

is( $d->tradition( $uuid ), $t, "Correct tradition returned for id" );
is( $d->stemma( $uuid ), $s, "Correct stemma returned for id" );
is( scalar $d->tradition_ids, 1, "Only one tradition in DB" );

# Connect to a new instance
my $e = Text::Tradition::Directory->new( 'dsn' => $dsn );
is( scalar $e->tradition_ids, 1, "One tradition preloaded from DB" );
my $te = $e->tradition( $uuid );
is( $te->name, $t->name, "New instance returns correct tradition" );
my $se = $e->stemma( $uuid );
is( $se->graph, $s->graph, "New instance returns correct stemma" );
is( $e->tradition( 'NOT-A-UUID' ), undef, "Undef returned for non-tradition" );
is( $e->stemma( 'NOT-A-UUID' ), undef, "Undef returned for non-stemma" );
$te->name( "Changed name" );
my $new_id = $e->save_tradition( $te );
is( $new_id, $uuid, "Updated tradition ID did not change" );

my $f = Text::Tradition::Directory->new( 'dsn' => $dsn, 'preload' => 0 );
is( scalar $f->tradition_ids, 0, "No traditions preloaded from DB" );
### TODO This doesn't work, as I cannot get an object scope in the
### 'tradition' wrapper.
# my $tf = $f->tradition( $uuid );
# is( $tf->name, $t->name, "Next instance returns correct tradition" );
# is( $tf->name, "Changed name", "Change to tradition carried through" );

=end testing

=cut

has data_hash => (
    traits => ['Hash'],
	default => sub { {} },
    handles => {
        tradition     => 'get',
        stemma		  => 'get',
        add_tradition => 'set',
        add_stemma	  => 'set',
        tradition_ids => 'keys',
    },
);
	
has +typemap => (
	is => 'rw',
	isa => 'KiokuDB::TypeMap',
	default => sub { 
		KiokuDB::TypeMap->new(
			isa_entries => {
				"Graph" => KiokuDB::TypeMap::Entry::Naive->new,
				"Graph::AdjacencyMap" => KiokuDB::TypeMap::Entry::Naive->new,
			}
		);
	},
);

has preload => (
	is => 'ro',
	isa => 'Bool',
	default => 1,
	);

around 'tradition' => sub {
	my( $orig, $self, @arg ) = @_;
	my $data = $self->$orig( @arg );
	unless( $data ) {
		# Connect to the DB and fetch the thing.
		$self->new_scope;
		my $id = shift @arg;
		my $trad = $self->lookup( $id );
		if( ref( $trad ) eq 'Text::Tradition' ) {
			$self->add_tradition( $id => $trad );
			return $trad;
		} 
		# If we got this far...
		return undef;
	}
	return $data->{'object'};
};

around 'stemma' => sub {
	my( $orig, $self, @arg ) = @_;
	my $data = $self->$orig( @arg );
	unless( $data ) {
		# Connect to the DB and fetch the thing.
		$self->new_scope;
		my $id = shift @arg;
		my $trad = $self->lookup( $id );
		if( ref( $trad ) eq 'Text::Tradition' ) {
			# Add it
			$self->add_tradition( $id => $trad );
			# Find the stemma whose collation belongs to $trad
			my $ret = $self->grep( sub { $_->collation eq $trad->collation } );
			my $stemma;
			until ( $ret->is_done ) {
				foreach my $st ( $ret->items ) {
					warn "Found two saved stemmas for tradition $id" if $stemma;
					$stemma = $st;
				}
			}
			if( $stemma ) {
				$self->add_stemma( $stemma );
				return $stemma;
			}
		} 
		# If we got this far...
		return undef;
	}
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
	
	$self->fetch_all if( $self->dsn && $self->preload );
}

# Connect to self, get the traditions and stemmas, and save them
# in the directory.
sub fetch_all {
	my $self = shift;
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
	foreach my $id ( $self->tradition_ids ) {
		my $c = $self->tradition( $id )->collation;
		if( exists $stemmata{$c} ) {
			$self->add_stemma( $id => $stemmata{$c} );
		}
	}
}
	

sub save_tradition {
	my( $self, $tradition ) = @_;
	# Write the thing to the db and return its ID.
	unless( ref( $tradition ) eq 'Text::Tradition' ) {
		warn "Object $tradition is not a Text::Tradition";
		return undef;
	}
	my $scope = $self->new_scope;
	my $uuid = $self->store( $tradition );
	$self->add_tradition( $uuid => $tradition );
	return $uuid;
}

sub save_stemma {
	my( $self, $stemma ) = @_;
	unless( ref( $stemma ) eq 'Text::Tradition::Stemma' ) {
		warn "Object $stemma is not a Text::Tradition::Stemma";
		return undef;
	}
	my $scope = $self->new_scope;
	# Get the tradition to which this stemma belongs.
	my $tradition = $stemma->collation->tradition;
	# Make sure the tradition is in the DB.
	my $tid = $self->save_tradition( $tradition );
	unless( $tid ) {
		warn "Could not access this stemma's tradition; aborting";
		return undef;
	}
	my $sid = $self->store( $stemma );
	$self->add_stemma( $tid => $stemma );
	return $tid;
}
	

1;
	
		