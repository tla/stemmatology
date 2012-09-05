#!/usr/bin/env perl

use strict;
use warnings;

use v5.10.0;

use Getopt::Long;
## using prompt():
use ExtUtils::MakeMaker();
use lib 'lib';

use Text::Tradition::Directory;

my ($dsn, $command) = ('dbi:SQLite:dbname=stemmaweb/db/traditions.db', 'add');
my ($username, $password, $tradition_id, $rolename, $dbuser, $dbpass);

GetOptions(
    'c|command:s' => \$command,
    'dsn:s' => \$dsn,
    'dbuser=s' => \$dbuser,
    'dbpass=s' => \$dbpass,
    'u|username=s' => \$username,
    'p|password:s' => \$password,
    't|tradition:s' => \$tradition_id,
    'r|role:s'      => \$rolename,
    ) or usage();

if(!$command || !($command ~~ [qw/add modify delete deactivate reactivate list/])) {
    print "No command supplied, chickening out ... \n\n";
    usage();
}

if(!$username) {
    print "No username supplied, confused ... \n\n";
    usage();
}

my %connect_args = ( dsn => $dsn );
if( $dbuser || $dbpass ) {
    $connect_args{extra_args} = { user => $dbuser, password => $dbpass };
}
my $userstore = Text::Tradition::Directory->new( %connect_args );
my $new_scope = $userstore->new_scope;

given ($command) {
    when ('add') {
        ## We only add local users here, OpenID etc users will get auto-added
        ## when they login
        if(!$password || !$userstore->validate_password($password)) {
            print "Can't add a new user without a valid password\n\n";
            usage();
            break;
        }
        ## Set role as passed in rolename, if set (else gets default "user")
        my $user = $userstore->add_user({ username => $username,
                                          password => $password,
                                          ( $rolename ? (role => $rolename) : () ),
                                      });
        if(!$user) {
            print "Failed to add user! (you should see errors above this..)\n";
        } else {
            print "OK.\n";
        }
    }

    when ('modify') {
        if(!$tradition_id && !$password && !$rolename) {
            print "Can't modify a user without a valid password or a tradition\n\n";
            usage();
            break;
        }
        if( $password && !$userstore->validate_password($password)) {
            print "Can't modify a user without a valid password\n\n";
            usage();
            break;
        }
        my @set_password = ( $password ? ( password => $password ) : () );
        my @set_role = ( $rolename ? ( role => $rolename ) : () );

        my $user = $userstore->modify_user({ username => $username, 
                                             @set_password,
                                             @set_role,
                                         });
        if(!$user) {
            print "Failed to modify user! (you should see errors above this..)\n";
        } else {
            print "Modified User.\n";
        }

        if($tradition_id) {
            my $tradition = $userstore->tradition($tradition_id);
            my $user = $userstore->find_user({ username => $username });
            if(!$tradition || !$user) {
                print "Can't find one of '$username' or '$tradition_id' in the database!\n";
            } else {
                if(grep { $userstore->object_to_id($_) 
                          eq 
                          $userstore->object_to_id($tradition)} 
                   @{$user->traditions}) {
                    $user->remove_tradition($tradition);
                } else {
                    $user->add_tradition($tradition);
                }
                $userstore->update($tradition);
                $userstore->update($user);
                print "Added Tradition.\n";
            }
        }

        print "OK\n";
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
    print "Usage: $0 -c modify -u jimbob -r \"admin\"\n";
    print "Usage: $0 -c deactivate -u jimbob\n";
}

=head1 NAME

admin_users.pl - add / modify / etc users

=head1 SYNOPSIS

    admin_user.pl -c add -u jimbob -p "jimspassword"

    admin_user.pl -c add -u jimbob -p "jimspassword" -r "admin"

    admin_user.pl -c modify -u jimbob -p "jimsnewpassword"

    admin_user.pl -c modify -u jimbob -r "admin"

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

=item -r | --role

A rolename to add or modify, this is a plain text string to check it carefully. Use C<modify> to change if necessary.

=back
