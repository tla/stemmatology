#!/usr/bin/perl

use strict;
use warnings;

use Test::More 'no_plan';
use KiokuX::Model;
use File::Temp;

use_ok('Text::Tradition::UserStore');

my $fh = File::Temp->new();
my $file = $fh->filename;
$fh->close;
my $dsn = "dbi:SQLite:dbname=$file";
my $d = KiokuX::Model->new( 'dsn' => $dsn,'extra_args' => { 'create' => 1 } );

my $user_store = Text::Tradition::UserStore->new(directory => $d);

my $new_user = $user_store->add_user('fred', 'bloggs');
isa_ok($new_user, 'Text::Tradition::User');

