#!/usr/bin/env perl

use strict;
use warnings;

use v5.10.0;

use Getopt::Long;
## using prompt():
use ExtUtils::MakeMaker();
use Text::Tradition::UserStore;

use lib 'lib';

my ($dsn, $command) = ('dbi:SQLite:dbname=db/traditions.db', 'add', undef);
my ($username, $password);

GetOptions(
    'c|command:s' => \$command,
    'dsn:s' => \$dsn,
    'u|username=s' => \$username,
    'p|password:s' => \$password,
    ) or usage();

if(!$command || !($command ~~ [qw/add modify delete deactivate reactivate/])) {
    print "No command supplied, chickening out ... \n\n";
    usage();
}

if(!$username) {
    print "No username supplied, confused ... \n\n";
    usage();
}

my $userstore = Text::Tradition::UserStore->new( dsn => $dsn);

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
        if(!$password || !$userstore->validate_password($password)) {
            print "Can't modify a user without a valid password\n\n";
            usage();
        }
        my $user = $userstore->modify_user({ username => $username, 
                                             password => $password });
        if(!$user) {
            print "Failed to modify user! (you should see errors above this..)\n";
        } else {
            print "OK.\n";
        }        
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
    print "Usage: $0 -c deactivate -u jimbob\n";
}

=head1 NAME

admin_users.pl - add / modify / etc users

=head1 SYNOPSIS

    admin_user.pl -c add -u jimbob -p "jimspassword"

    admin_user.pl -c modify -u jimbob -p "jimsnewpassword"

    admin_user.pl -c delete -u jimbob

=head1 OPTIONS

=over

=item -c | --command

The action to take, can be one of: add, modify, deactivate, reactivate, delete.

=item -u | --username

The username of the new user or user to change.

=item -p | --password

The new password or password to change.

=back
