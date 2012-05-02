#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
binmode STDOUT, ':utf8';
use Text::Tradition;
use_ok( 'Text::Tradition::Language::English' );
}




1;
