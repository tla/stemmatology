package Text::Tradition::Directory;

use strict;
use warnings;
use Moose;
use DBI;
use Encode qw/ decode_utf8 /;
use KiokuDB::GC::Naive;
use KiokuDB::TypeMap;
use KiokuDB::TypeMap::Entry::Naive;
use Text::Tradition::Error;

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
  my $stemma = $tradition->add_stemma( dotfile => $dotfile ); 
  $d->save_tradition( $tradition );
  
  foreach my $id ( $d->traditions ) {
  	print $d->tradition( $id )->name;
  }
    
=head1 DESCRIPTION

Text::Tradition::Directory is an interface for storing and retrieving text traditions and all their data, including an associated stemma hypothesis.  It is an instantiation of a KiokuDB::Model, storing traditions and associated stemmas by UUID.

=head1 METHODS

=head2 new

Returns a Directory object. 

=head2 traditionlist

Returns a hashref mapping of ID => name for all traditions in the directory.

=head2 tradition( $id )

Returns the Text::Tradition object of the given ID.

=head2 save( $tradition )

Writes the given tradition to the database, returning its ID.

=head2 delete( $tradition )

Deletes the given tradition object from the database.
WARNING!! Garbage collection does not yet work. Use this sparingly.

=begin testing

use TryCatch;
use File::Temp;
use Text::Tradition;
use_ok 'Text::Tradition::Directory';

my $fh = File::Temp->new();
my $file = $fh->filename;
$fh->close;
my $dsn = "dbi:SQLite:dbname=$file";
my $uuid;
my $t = Text::Tradition->new( 
	'name'  => 'inline', 
	'input' => 'Tabular',
	'file'  => 't/data/simple.txt',
	);

{
	my $d = Text::Tradition::Directory->new( 'dsn' => $dsn,
		'extra_args' => { 'create' => 1 } );
	is( ref $d, 'Text::Tradition::Directory', "Got directory object" );
	
	my $scope = $d->new_scope;
	$uuid = $d->save( $t );
	ok( $uuid, "Saved test tradition" );
	
	my $s = $t->add_stemma( dotfile => 't/data/simple.dot' );
	ok( $d->save( $t ), "Updated tradition with stemma" );
	is( $d->tradition( $uuid ), $t, "Correct tradition returned for id" );
	is( $d->tradition( $uuid )->stemma(0), $s, "...and it has the correct stemma" );
	try {
		$d->save( $s );
	} catch( Text::Tradition::Error $e ) {
		is( $e->ident, 'database error', "Got exception trying to save stemma directly" );
		like( $e->message, qr/Cannot directly save non-Tradition object/, 
			"Exception has correct message" );
	}
}
my $nt = Text::Tradition->new(
	'name' => 'CX',
	'input' => 'CollateX',
	'file' => 't/data/Collatex-16.xml',
	);
is( ref( $nt ), 'Text::Tradition', "Made new tradition" );

{
	my $f = Text::Tradition::Directory->new( 'dsn' => $dsn );
	my $scope = $f->new_scope;
	is( scalar $f->traditionlist, 1, "Directory index has our tradition" );
	my $nuuid = $f->save( $nt );
	ok( $nuuid, "Stored second tradition" );
	my @tlist = $f->traditionlist;
	is( scalar @tlist, 2, "Directory index has both traditions" );
	my $tf = $f->tradition( $uuid );
	my( $tlobj ) = grep { $_->{'id'} eq $uuid } @tlist;
	is( $tlobj->{'name'}, $tf->name, "Directory index has correct tradition name" );
	is( $tf->name, $t->name, "Retrieved the tradition from a new directory" );
	my $sid = $f->object_to_id( $tf->stemma(0) );
	try {
		$f->tradition( $sid );
	} catch( Text::Tradition::Error $e ) {
		is( $e->ident, 'database error', "Got exception trying to fetch stemma directly" );
		like( $e->message, qr/not a Text::Tradition/, "Exception has correct message" );
	}
	try {
		$f->delete( $sid );
	} catch( Text::Tradition::Error $e ) {
		is( $e->ident, 'database error', "Got exception trying to delete stemma directly" );
		like( $e->message, qr/Cannot directly delete non-Tradition object/, 
			"Exception has correct message" );
	}
	
	$f->delete( $uuid );
	ok( !$f->exists( $uuid ), "Object is deleted from DB" );
	ok( !$f->exists( $sid ), "Object stemma also deleted from DB" );
	is( scalar $f->traditionlist, 1, "Object is deleted from index" );
}

{
	my $g = Text::Tradition::Directory->new( 'dsn' => $dsn );
	my $scope = $g->new_scope;
	is( scalar $g->traditionlist, 1, "Now one object in new directory index" );
	my $ntobj = $g->tradition( 'CX' );
	my @w1 = sort { $a->sigil cmp $b->sigil } $ntobj->witnesses;
	my @w2 = sort{ $a->sigil cmp $b->sigil } $nt->witnesses;
	is_deeply( \@w1, \@w2, "Looked up remaining tradition by name" );
}

=end testing

=cut
use Text::Tradition::TypeMap::Entry;

has +typemap => (
  is      => 'rw',
  isa     => 'KiokuDB::TypeMap',
  default => sub {
    KiokuDB::TypeMap->new(
      isa_entries => {
        "Text::Tradition" =>
          KiokuDB::TypeMap::Entry::Naive->new(),
        "Graph" => Text::Tradition::TypeMap::Entry->new(),
        "Graph::AdjacencyMap" => Text::Tradition::TypeMap::Entry->new(),
      }
    );
  },
);

# Push some columns into the extra_args
around BUILDARGS => sub {
	my $orig = shift;
	my $class = shift;
	my $args;
	if( @_ == 1 ) {
		$args = $_[0];
	} else {
		$args = { @_ };
	}
	if( $args->{'dsn'} =~ /^dbi/ ) { # We're using Backend::DBI
		my @column_args = ( 'columns',
			[ 'name' => { 'data_type' => 'varchar', 'is_nullable' => 1 } ] );
		my $ea = $args->{'extra_args'};
		if( ref( $ea ) eq 'ARRAY' ) {
			push( @$ea, @column_args );
		} elsif( ref( $ea ) eq 'HASH' ) {
			$ea = { %$ea, @column_args };
		} else {
			$ea = { @column_args };
		}
		$args->{'extra_args'} = $ea;
	}
	return $class->$orig( $args );
};

# before [ qw/ store update insert delete / ] => sub {
before [ qw/ delete / ] => sub {
	my $self = shift;
	my @nontrad;
	foreach my $obj ( @_ ) {
		if( ref( $obj ) && ref( $obj ) ne 'Text::Tradition' ) {
			# Is it an id => Tradition hash?
			if( ref( $obj ) eq 'HASH' && keys( %$obj ) == 1 ) {
				my( $k ) = keys %$obj;
				next if ref( $obj->{$k} ) eq 'Text::Tradition';
			}
			push( @nontrad, $obj );
		}
	}
	if( @nontrad ) {
		throw( "Cannot directly save non-Tradition object of type "
			. ref( $nontrad[0] ) );
	}
};

# TODO Garbage collection doesn't work. Suck it up and live with the 
# inflated DB.
after delete => sub {
	my $self = shift;
	my $gc = KiokuDB::GC::Naive->new( backend => $self->directory->backend );
	$self->directory->backend->delete( $gc->garbage->members );
};

sub save {
	my $self = shift;
	return $self->store( @_ );
}

sub tradition {
	my( $self, $id ) = @_;
	my $obj = $self->lookup( $id );
	unless( $obj ) {
		# Try looking up by name.
		foreach my $item ( $self->traditionlist ) {
			if( $item->{'name'} eq $id ) {
				$obj = $self->lookup( $item->{'id'} );
				last;
			}
		}
	}
	if( $obj && ref( $obj ) ne 'Text::Tradition' ) {
		throw( "Retrieved object is a " . ref( $obj ) . ", not a Text::Tradition" );
	}
	return $obj;
}

sub traditionlist {
	my $self = shift;
	# If we are using DBI, we can do it the easy way; if not, the hard way.
	# Easy way still involves making a separate DBI connection. Ew.
	my @tlist;
	if( $self->dsn =~ /^dbi:(\w+):/ ) {
		my $dbtype = $1;
		my @connection = @{$self->directory->backend->connect_info};
		# Get rid of KiokuDB-specific arg
		pop @connection if scalar @connection > 4;
		$connection[3]->{'sqlite_unicode'} = 1 if $dbtype eq 'SQLite';
		$connection[3]->{'pg_enable_utf8'} = 1 if $dbtype eq 'Pg';
		my $dbh = DBI->connect( @connection );
		my $q = $dbh->prepare( 'SELECT id, name from entries WHERE class = "Text::Tradition"' );
		$q->execute();
		while( my @row = $q->fetchrow_array ) {
			my( $id, $name ) = @row;
			# Horrible horrible hack
			$name = decode_utf8( $name ) if $dbtype eq 'mysql';
			push( @tlist, { 'id' => $row[0], 'name' => $row[1] } );
		}
	} else {
		$self->scan( sub { my $o = shift; 
						   push( @tlist, { 'id' => $self->object_to_id( $o ), 
										   'name' => $o->name } ) } );
	}
	return @tlist;
}

sub throw {
	Text::Tradition::Error->throw( 
		'ident' => 'database error',
		'message' => $_[0],
		);
}

1;
	
=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews E<lt>aurum@cpan.orgE<gt>
