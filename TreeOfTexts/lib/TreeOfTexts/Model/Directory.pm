package TreeOfTexts::Model::Directory;
use strict;
use warnings;
use Moose;
use Text::Tradition::Directory;

extends 'Catalyst::Model::KiokuDB';

has '+model_class' => ( default => 'Text::Tradition::Directory' );

1;
