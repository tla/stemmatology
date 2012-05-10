package Text::Tradition::UserStore;

use strict;
use warnings;

use Moose;
use KiokuX::User::Util qw(crypt_password);

extends 'KiokuX::Model';

use Text::Tradition::User;
# use Text::Tradition::Directory;

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
sub add_user {
    my ($self, $userinfo) = @_;
    my $username = $userinfo->{username};
    my $password = $userinfo->{password};

    return unless $username && $self->validate_password($password);

    my $user = Text::Tradition::User->new(
        id => $username,
        password => crypt_password($password),
    );

    my $scope = $self->new_scope;
    $self->store($user->kiokudb_object_id, $user);

    return $user;
}

sub find_user {
    my ($self, $userinfo) = @_;
    my $username = $userinfo->{username};

    my $scope = $self->new_scope;
    return $self->lookup(Text::Tradition::User->id_for_user($username));
    
}

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

sub validate_password {
    my ($self, $password) = @_;

    return if !$password;
    return if length($password) < $self->MIN_PASS_LEN;

    return 1;
}

1;
