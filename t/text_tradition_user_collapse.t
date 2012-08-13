#!/usr/bin/perl

use strict;
use warnings;

use Test::More 'no_plan';
use File::Temp;
use Text::Tradition;

use_ok('Text::Tradition::Directory');

my $fh   = File::Temp->new();
my $file = $fh->filename;
$fh->close;
my $dsn = "dbi:SQLite:dbname=$file";

my $user_store = Text::Tradition::Directory->new(
  'dsn'        => $dsn,
  'extra_args' => { 'create' => 1 }
);

my $uuid;
my $email = 'john@doe.com';
{
  my $scope = $user_store->new_scope;

## create user
  my $new_user = $user_store->add_user(
    { username => 'fred',
      password => 'bloggspass'
    }
  );

  my $t = Text::Tradition->new(
    'name'  => 'inline',
    'input' => 'Tabular',
    'file'  => 't/data/simple.txt',
  );

  $uuid = $user_store->save($t);
  $new_user->add_tradition($t);
  $new_user->email($email);
  $user_store->update($new_user);
}

{
  my $scope = $user_store->new_scope;

  # change attribute in the user object
  my $user = $user_store->find_user( { username => 'fred' } );
  $user->email('foo@bar.baz');
  $user_store->update($user);
  is(scalar @{$user->traditions}, 1);
}

{
  my $scope = $user_store->new_scope;

  # refetch tradition
  my $fetched_t = $user_store->tradition($uuid);

  # assert that the associated user also changed
  is( $fetched_t->user->email, 'foo@bar.baz' );
}

{
  my $scope = $user_store->new_scope;
  my $user = $user_store->find_user( { username => 'fred' } );

  # change the email back to what it was
  $user->email($email);
  $user_store->update($user);
}

{
  my $scope = $user_store->new_scope;

  # refetch tradition
  my $fetched_t = $user_store->tradition( $uuid );

  # assert that email has actually been reverted
  is( $fetched_t->user->email, $email );
}
