#!/usr/bin/perl

use strict;
use warnings;

use Test::More 'no_plan';
use File::Temp;

use_ok('Text::Tradition::Directory');

my $fh = File::Temp->new();
my $file = $fh->filename;
$fh->close;
my $dsn = "dbi:SQLite:dbname=$file";

my $user_store = Text::Tradition::Directory->new('dsn' => $dsn,
                                                 'extra_args' => { 'create' => 1 } );

my $scope = $user_store->new_scope;

## passwords
my $shortpass = 'bloggs';
ok(!$user_store->validate_password($shortpass), '"bloggs" is too short for a password');

## create user
my $new_user = $user_store->add_user({ username => 'fred',
                                       password => 'bloggspass'});
isa_ok($new_user, 'Text::Tradition::User');
is($new_user->active, 1, 'New user created and active');

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

{
## deactivate user
## Sets all traditions to non-public, deactivates
    my $user = $user_store->add_user({ username => 'testactive',
                                       password => 'imanactiveuser' });
    ok($user->active, 'Deactivate test user starts active');

    my $d_user = $user_store->deactivate_user({ username => 'testactive' });
    is($d_user->active, 0, 'Deactivated user');

## TODO - add test where user has traditions to start with
}

{
## reactivate user
## reactivates user, does not mess with their traditions (as we don't know which were public to start with)

    my $user = $user_store->add_user({ username => 'testinactive',
                                       password => 'imaninactiveuser' });
    my $d_user = $user_store->deactivate_user({ username => 'testactive' });
    ok(!$d_user->active, 'Deactivate test user starts active');   
    
    my $a_user = $user_store->reactivate_user({ username => 'testinactive' });
    is($a_user->active, 1, 'Re-activated user');
}

{
## delete user (admin only?)
    my $user = $user_store->add_user({ username => 'testdelete',
                                       password => 'imgoingtobedeleted' });

    my $gone = $user_store->delete_user({ username => 'testdelete' });

    my $d_user = $user_store->find_user({ username => 'testdelete' });

    ok($gone && !$d_user, 'Deleted user completely from store');
}

{
## add_tradition
    use Text::Tradition;
    my $t = Text::Tradition->new( 
        'name'  => 'inline', 
        'input' => 'Tabular',
        'file'  => 't/data/simple.txt',
	);

    my $uuid = $user_store->save($t);
    my $user = $user_store->add_user({ username => 'testadd',
                                       password => 'testingtraditions' });
    $user->add_tradition($t);
    $user_store->update($user);
#     $userstore->update($t);

    is( scalar @{$user->traditions}, 1, 'Added one tradition');

    my @tlist = $user_store->traditionlist($user->kiokudb_object_id);
    is($tlist[0]->name, $t->name, 'Traditionlist returns stored user->tradition');
}

