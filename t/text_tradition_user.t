#!/usr/bin/perl

use strict;
use warnings;

use Test::More 'no_plan';
# use KiokuX::Model;
use File::Temp;

use_ok('Text::Tradition::UserStore');

my $fh = File::Temp->new();
my $file = $fh->filename;
$fh->close;
my $dsn = "dbi:SQLite:dbname=$file";
# my $d = KiokuX::Model->new( 'dsn' => $dsn,'extra_args' => { 'create' => 1 } );

my $user_store = Text::Tradition::UserStore->new('dsn' => $dsn,'extra_args' => { 'create' => 1 } );

## create user
my $new_user = $user_store->add_user('fred', 'bloggs');
isa_ok($new_user, 'Text::Tradition::User');

## find user
my $find_user = $user_store->find_user({ username => 'fred'});
isa_ok($find_user, 'Text::Tradition::User');
ok($find_user->check_password('bloggs'), 'Stored & retrieved with correct password');
 


