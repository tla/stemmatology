package Text::Tradition::User;

use strict;
use warnings;

use Moose;
with qw(KiokuX::User);

## 'id' provided by KiokuX::User stores our username
has 'password'   => (is => 'rw', required => 1);
has 'active'     => (is => 'rw', default => sub { 1; });
# 'traits' => ['Array'] ?
# https://metacpan.org/module/Moose::Meta::Attribute::Native::Trait::Array
has 'traditions' => (is => 'rw', isa => 'ArrayRef[Text::Tradition]', required => 0);

# after add_tradition => sub { 
#     $tradition->set_user($self) 
#         unless $tradition->user->id eq $self->id;
# }

1;

=head1 NAME

Text::Tradition::User - Users which own traditions, and can login to the web app

=head1 SYNOPSIS

    ## Users are managed by Text::Tradition::UserStore

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

    foreach my $t (@{ $user->traditions }) {
      ## do something with traditions owned by this user.
    }

=head1 DESCRIPTION

User objects representing owners of L<Text::Tradition>s and authenticated users.

=head2 ATTRIBUTES

=head3 id

Inherited from KiokuX::User, stores the 'username' (login) of the user.

=head3 password

User's password, encrypted on creation (by
L<KiokuX::User::Util/crypt_password>.

=head3 active

Active flag, defaults to true (1). Will be set to false (0) by
L<Text::Tradition::UserStore/deactivate_user>.

=head3 traditions

Returns an ArrayRef of L<Text::Tradition> objects belonging to this user.

=head2 METHODS

=head3 check_password

Inherited from KiokuX::User, verifies a given password string against
the stored encrypted version.
