package Text::Tradition::UserStore;

use strict;
use warnings;

use Moose;
use KiokuX::User::Util qw(crypt_password);

extends 'KiokuX::Model';

use Text::Tradition::User;
# use Text::Tradition::Directory;

=head1 NAME

Text::Tradition::UserStore - KiokuDB storage management for Users

=head1 SYNOPSIS

    my $userstore = Text::Tradition::UserStore->new(dsn => 'dbi:SQLite:foo.db');
    my $newuser = $userstore->add_user({ username => 'fred',
                                         password => 'somepassword' });

    my $fetchuser = $userstore->find_user({ username => 'fred' });
    if($fetchuser->check_password('somepassword')) { 
       ## login user or .. whatever
    }

    my $user = $userstore->deactivate_user({ username => 'fred' });
    if(!$user->active) { 
      ## shouldnt be able to login etc
    }

=head1 DESCRIPTION

A L<KiokuX::Model> for managing the storage and creation of
L<Text::Tradition::User> objects. Subclass or replace this module in
order to use a different source for stemmaweb users.

=head2 ATTRIBUTES

=head3 dsn

Inherited from KiokuX::Model - dsn for the data store we are using. 

=head3 MIN_PASS_LEN

Constant for the minimum password length when validating passwords,
defaults to "8".

=cut

has MIN_PASS_LEN => ( is => 'ro', isa => 'Num', default => sub { 8 } );

# has 'directory' => ( 
#     is => 'rw', 
#     isa => 'KiokuX::Model',
#     handles => []
#     );

## TODO: Some of these methods should probably optionally take $user objects
## instead of hashrefs.

## It also occurs to me that all these methods don't need to be named
## XX_user, but leaving that way for now incase we merge this code
## into ::Directory for one-store.

## To die or not to die, on error, this is the question.

=head2 METHODS

=head3 add_user

Takes a hashref of C<username>, C<password>.

Create a new user object, store in the KiokuDB backend, and return it.

=cut

sub add_user {
    my ($self, $userinfo) = @_;
    my $username = $userinfo->{url} || $userinfo->{username};
    my $password = $userinfo->{password};

    return unless ($username =~ /^https?:/ 
                   || ($username && $self->validate_password($password))) ;

    my $user = Text::Tradition::User->new(
        id => $username,
        password => ($password ? crypt_password($password) : ''),
    );

    my $scope = $self->new_scope;
    $self->store($user->kiokudb_object_id, $user);

    return $user;
}

sub create_user {
    my $self = shift;
    return $self->add_user(@_);
}

=head3 find_user

Takes a hashref of C<username>, optionally C<openid_identifier>.

Fetches the user object for the given username and returns it.

=cut

sub find_user {
    my ($self, $userinfo) = @_;
    ## url or display?
    # 'display' => 'castaway.myopenid.com',
    # 'url' => 'http://castaway.myopenid.com/',
    my $username = $userinfo->{url} || $userinfo->{username};

    my $scope = $self->new_scope;
    return $self->lookup(Text::Tradition::User->id_for_user($username));
    
}

=head3 modify_user

Takes a hashref of C<username> and C<password> (same as add_user).

Retrieves the user, and updates it with the new information. Username
changing is not currently supported.

Returns the updated user object, or undef if not found.

=cut

sub modify_user {
    my ($self, $userinfo) = @_;
    my $username = $userinfo->{username};
    my $password = $userinfo->{password};

    return unless $username && $self->validate_password($password);

    my $user = $self->find_user({ username => $username });
    return unless $user;

    my $scope = $self->new_scope;
    $user->password(crypt_password($password));

    $self->update($user);

    return $user;
}

=head3 deactivate_user

Takes a hashref of C<username>.

Sets the users C<active> flag to false (0), and sets all traditions
assigned to them to non-public, updates the storage and returns the
deactivated user.

Returns undef if user not found.

=cut

sub deactivate_user {
    my ($self, $userinfo) = @_;
    my $username = $userinfo->{username};

    return if !$username;

    my $user = $self->find_user({ username => $username });
    return if !$user;

    $user->active(0);
    foreach my $tradition (@{ $user->traditions }) {
        ## Not implemented yet
        # $tradition->public(0);
    }
    my $scope = $self->new_scope;

    ## Should we be using Text::Tradition::Directory also?
    $self->update(@{ $user->traditions });

    $self->update($user);

    return $user;
}

=head3 reactivate_user

Takes a hashref of C<username>.

Returns the user object if already activated. Activates (sets the
active flag to true (1)), updates the storage and returns the user.

Returns undef if the user is not found.

=cut

sub reactivate_user {
    my ($self, $userinfo) = @_;
    my $username = $userinfo->{username};

    return if !$username;

    my $user = $self->find_user({ username => $username });
    return if !$user;

    return $user if $user->active;

    $user->active(1);
    my $scope = $self->new_scope;
    $self->update($user);

    return $user;    
}

=head3 delete_user

CAUTION: Delets actual data!

Takes a hashref of C<username>.

Returns undef if the user doesn't exist.

Removes the user from the store and returns 1.

=cut

sub delete_user {
    my ($self, $userinfo) = @_;
    my $username = $userinfo->{username};

    return if !$username;

    my $user = $self->find_user({ username => $username });
    return if !$user;

    my $scope = $self->new_scope;

    ## Should we be using Text::Tradition::Directory for this bit?
    $self->delete( @{ $user->traditions });

    ## Poof, gone.
    $self->delete($user);

    return 1;
}

=head3 validate_password

Takes a password string. Returns true if it is longer than
L</MIN_PASS_LEN>, false otherwise.

Used internally by L</add_user>.

=cut

sub validate_password {
    my ($self, $password) = @_;

    return if !$password;
    return if length($password) < $self->MIN_PASS_LEN;

    return 1;
}

1;
