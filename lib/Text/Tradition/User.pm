package Text::Tradition::User;

use strict;
use warnings;

use Moose;
with qw(KiokuX::User);

has 'password'   => (is => 'rw', required => 1);
has 'traditions' => (is => 'rw', isa => 'ArrayRef[Text::Tradition]', required => 0);

# after add_tradition => sub { 
#     $tradition->set_user($self) 
#         unless $tradition->user->username eq $self->name;
# }

1;
