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
  my $stemma = $tradition->add_stemma( $dotfile ); 
  $d->save_tradition( $tradition );
  
  foreach my $id ( $d->traditions ) {
  	print $d->tradition( $id )->name;
  }
    
=head1 DESCRIPTION

Text::Tradition::Directory is an interface for storing and retrieving text traditions and all their data, including an associated stemma hypothesis.  It is an instantiation of a KiokuDB::Model, storing traditions and associated stemmas by UUID.

=head1 METHODS

=head2 new

Returns a Directory object. 

=head2 tradition_ids

Returns the ID of all traditions in the database.

=head2 tradition( $id )

Returns the Text::Tradition object of the given ID.

=head2 save( $tradition )

Writes the given tradition to the database, returning its ID.

=begin testing

use Test::Warn;
use File::Temp;
use Text::Tradition;
use_ok 'Text::Tradition::Directory';

my $fh = File::Temp->new();
my $file = $fh->filename;
$fh->close;
my $dsn = "dbi:SQLite:dbname=$file";

my $d = Text::Tradition::Directory->new( 'dsn' => $dsn,
	'extra_args' => { 'create' => 1 } );
is( ref $d, 'Text::Tradition::Directory', "Got directory object" );

my $scope = $d->new_scope;
my $t = Text::Tradition->new( 
	'name'  => 'inline', 
	'input' => 'Tabular',
	'file'  => 't/data/simple.txt',
	);
my $uuid = $d->save( $t );
ok( $uuid, "Saved test tradition" );

my $s = $t->add_stemma( 't/data/simple.dot' );
ok( $d->save( $t ), "Updated tradition with stemma" );
is( $d->tradition( $uuid ), $t, "Correct tradition returned for id" );
is( $d->tradition( $uuid )->stemma, $s, "...and it has the correct stemma" );
warning_like { $d->save( $s ) } qr/not a Text::Tradition/, "Correctly failed to save stemma directly";

my $e = Text::Tradition::Directory->new( 'dsn' => $dsn );
$scope = $e->new_scope;
is( scalar $e->tradition_ids, 1, "Directory index has our tradition" );
my $te = $e->tradition( $uuid );
is( $te->name, $t->name, "Retrieved the tradition from a new directory" );
my $sid = $e->object_to_id( $te->stemma );
warning_like { $e->tradition( $sid ) } qr/not a Text::Tradition/, "Did not retrieve stemma via tradition call";
warning_like { $e->delete( $sid ) } qr/Cannot directly delete non-Tradition object/, "Stemma object not deleted from DB";
$e->delete( $uuid );
ok( !$e->exists( $uuid ), "Object is deleted from DB" );
is( scalar $e->tradition_ids, 0, "Object is deleted from index" );


=end testing

=cut

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

has tradition_index => (
    traits => ['Hash'],
    isa => 'HashRef[Str]',
    handles => {
        add_index		=> 'set',
        del_index		=> 'delete',
        name			=> 'get',
        tradition_ids	=> 'keys',
    },
    default => sub { {} },
    );

# Populate the tradition index.
sub BUILD {
	my $self = shift;
	my $stream = $self->root_set;
	until( $stream->is_done ) {
		foreach my $obj ( $stream->items ) {
			my $uuid = $self->object_to_id( $obj );
			if( ref( $obj ) eq 'Text::Tradition' ) {
				 $self->add_index( $uuid => $obj->name );
			} else {
				warn "Found root object in DB that is not a Text::Tradition";
			}
		}
	}
	return $self;
}

# If a tradition is deleted, remove it from the index.
around delete => sub {
	my $orig = shift;
	my $self = shift;
	warn "Only the first object will be deleted" if @_ > 1;
	my $arg = shift;
	my $obj = ref( $arg ) ? $arg : $self->lookup( $arg );
	my $id = ref( $arg ) ? $self->object_to_id( $arg ) : $arg;
	unless( ref $obj eq 'Text::Tradition' ) {
		warn "Cannot directly delete non-Tradition object $obj";
		return;
	}
	$self->$orig( $arg );
	$self->del_index( $id );
};

sub save {
	my( $self, $obj ) = @_;
	unless( ref( $obj ) eq 'Text::Tradition' ) {
		warn "Object $obj is not a Text::Tradition";
		return;
	}
	my $uuid = $self->store( $obj );
	$self->add_index( $uuid => $obj->name ) if $uuid;
	return $uuid;
}


sub tradition {
	my( $self, $id ) = @_;
	my $obj = $self->lookup( $id );
	unless( ref( $obj ) eq 'Text::Tradition' ) {
		warn "Retrieved object is a " . ref( $obj ) . ", not a Text::Tradition";
		return;
	}
	return $obj;
}

1;
	
		