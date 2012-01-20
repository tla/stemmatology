#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use Text::Tradition;

use_ok( 'Text::Tradition::Collation::RelationshipStore' );
}




1;
