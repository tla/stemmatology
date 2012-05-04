package Text::Tradition::UserStore;

use strict;
use warnings;

use Moose;
use KiokuX::User::Util qw(crypt_password);

extends 'KiokuX::Model';

use Text::Tradition::User;

# has 'directory' => ( 
#     is => 'rw', 
#     isa => 'KiokuX::Model',
#     handles => []
#     );

sub add_user {
    my ($self, $username, $password) = @_;

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

    return $self->lookup(Text::Tradition::User->id_for_user($username));
    
}

1;
