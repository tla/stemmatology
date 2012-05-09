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

my $user_store = Text::Tradition::UserStore->new('dsn' => $dsn,
                                                 'extra_args' => { 'create' => 1 } );

## passwords
my $shortpass = 'bloggs';
ok(!$user_store->validate_password($shortpass), '"bloggs" is too short for a password');

## create user
my $new_user = $user_store->add_user({ username => 'fred',
                                       password => 'bloggspass'});
isa_ok($new_user, 'Text::Tradition::User');

## find user
my $find_user = $user_store->find_user({ username => 'fred'});
isa_ok($find_user, 'Text::Tradition::User');
ok($find_user->check_password('bloggspass'), 'Stored & retrieved with correct password');

## modify user
my $changed_user = $user_store->modify_user({ username => 'fred',
                                              password => 'passbloggs' });
isa_ok($changed_user, 'Text::Tradition::User');
my $changed = $user_store->find_user({ username => 'fred'});
ok($changed->check_password('passbloggs'), 'Modified & retrieved with correct new password');


