#!/usr/bin/env perl

use strict;
use warnings;

use v5.10.0;

use Getopt::Long;
## using prompt():
use ExtUtils::MakeMaker();
use lib 'lib';

use Text::Tradition::Directory;

my ($dsn, $command) = ('dbi:SQLite:dbname=db/traditions.db', 'add', undef);
my ($username, $password, $tradition_id);

GetOptions(
    'c|command:s' => \$command,
    'dsn:s' => \$dsn,
    'u|username=s' => \$username,
    'p|password:s' => \$password,
    't|tradition:s' => \$tradition_id,
    ) or usage();

if(!$command || !($command ~~ [qw/add modify delete deactivate reactivate list/])) {
    print "No command supplied, chickening out ... \n\n";
    usage();
}

if(!$username) {
    print "No username supplied, confused ... \n\n";
    usage();
}

# my $userstore = Text::Tradition::UserStore->new( dsn => $dsn);
my $userstore = Text::Tradition::Directory->new( dsn => $dsn);
my $new_scope = $userstore->new_scope;

given ($command) {
    when ('add') {
        if(!$password || !$userstore->validate_password($password)) {
            print "Can't add a new user without a valid password\n\n";
            usage();
        }
        my $user = $userstore->add_user({ username => $username, 
                                          password => $password });
        if(!$user) {
            print "Failed to add user! (you should see errors above this..)\n";
        } else {
            print "OK.\n";
        }
    }

    when ('modify') {
        if(!$tradition_id && !$password) {
            print "Can't modify a user without a valid password or a tradition\n\n";
            usage();
            break;
        }
        if( $password && !$userstore->validate_password($password)) {
            print "Can't modify a user without a valid password\n\n";
            usage();
            break;
        }
        if($password) {
            my $user = $userstore->modify_user({ username => $username, 
                                                 password => $password });
            if(!$user) {
                print "Failed to modify user! (you should see errors above this..)\n";
            } else {
                print "OK.\n";
            }
        } elsif($tradition_id) {
            my $tradition = $userstore->tradition($tradition_id);
            my $user = $userstore->find_user({ username => $username });
            if(!$tradition || !$user) {
                print "Can't find one of '$username' or '$tradition_id' in the database!\n";
            } else {
                $user->add_tradition($tradition);
                $userstore->update($tradition);
                $userstore->update($user);
                print "OK.\n";
            }
        } 
    }

    when ('list') {
        my $user = $userstore->find_user({ username => $username });
        if(!$user) {
            print "Can't find user '$username'\n";
            break;
        }
        my $traditions = $user->traditions;

        print "User: $username\n";
        print "Has traditions: \n";
        foreach my $t (@$traditions) {
            print "    ", $t->name, "\n";
        }
        print "OK.\n";
    }

    when ('deactivate') {
        my $user = $userstore->deactivate_user({ username => $username});
        if(!$user) {
            print "Failed to deactivate user! (you should see errors above this..)\n";
        } else {
            print "OK.\n";
        }        
    }

    when ('reactivate') {
        my $user = $userstore->reactivate_user({ username => $username});
        if(!$user) {
            print "Failed to reactivate user! (you should see errors above this..)\n";
        } else {
            print "OK.\n";
        }        
    }

    when ('delete') {
        my $yesno = ExtUtils::MakeMaker::prompt("Permanently delete $username? (y/N)", "n");
        if($yesno !~ /^y$/i) {
            print "Not deleting $username\n";
            break;
        }
        my $user = $userstore->delete_user({ username => $username});
        if(!$user) {
            print "Failed to delete user! (you should see errors above this..)\n";
        } else {
            print "OK.\n";
        }        
    }
}

sub usage {
    print "User Admin tool, to add/modify/deactivate/reactivate/delete users\n";
    print "===========================================\n";
    print "Usage: $0 -c add -u jimbob -p hispassword\n";
    print "Usage: $0 -c modify -u jimbob -p hisnewpassword\n";
    print "Usage: $0 -c modify -u jimbob -t \"Notre besoin\"\n";
    print "Usage: $0 -c deactivate -u jimbob\n";
}

=head1 NAME

admin_users.pl - add / modify / etc users

=head1 SYNOPSIS

    admin_user.pl -c add -u jimbob -p "jimspassword"

    admin_user.pl -c modify -u jimbob -p "jimsnewpassword"

    admin_user.pl -c modify -u jimbob -t "mytradition"

    admin_user.pl -c list -u jimbob

    admin_user.pl -c delete -u jimbob

=head1 OPTIONS

=over

=item -c | --command

The action to take, can be one of: add, modify, deactivate, reactivate, delete, list.

=over

=item add

Create a new user and store it in the Directory

=item modify

Change an existing stored user, with a -p this will change the user's
password, with a -t will add or remove the named tradition from the
user.

=item list

List the given user's traditions.

=item deactivate

Deactivate this user.

=item reactivate

Re-activate this user.

=item delete

Delete the user permanently.

=back

=item -u | --username

The username of the new user or user to change.

=item -p | --password

The new password or password to change.

=item -t | --tradition

A Text::Tradition id or name which will be assigned to the user given. 

=back
