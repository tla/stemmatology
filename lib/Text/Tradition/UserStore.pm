package Text::Tradition::UserStore;

use strict;
use warnings;

use Moose;
use KiokuX::User::Util qw(crypt_password);

use Text::Tradition::User;
use Text::Tradition::Directory;

has 'directory' => ( is => 'rw', isa => 'KiokuX::Model');

sub add_user {
    my ($self, $username, $password) = @_;

    my $user = Text::Tradition::User->new(
        id => $username,
        password => crypt_password($password),
    );

    my $scope = $self->directory->new_scope;
    $self->directory->store($user->kiokudb_object_id, $user);

    return $user;
}

sub find_user {
    my ($self, $username) = @_;

    return $self->directory->lookup($self->user_prefix . $username);
    
}

1;
