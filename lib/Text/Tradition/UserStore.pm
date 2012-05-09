package Text::Tradition::UserStore;

use strict;
use warnings;

use Moose;
use KiokuX::User::Util qw(crypt_password);

extends 'KiokuX::Model';

use Text::Tradition::User;

has MIN_PASS_LEN => ( is => 'ro', isa => 'Num', default => sub { 8 } );

# has 'directory' => ( 
#     is => 'rw', 
#     isa => 'KiokuX::Model',
#     handles => []
#     );

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

sub delete_user {
}


sub validate_password {
    my ($self, $password) = @_;

    return if !$password;
    return if length($password) < $self->MIN_PASS_LEN;

    return 1;
}

1;
