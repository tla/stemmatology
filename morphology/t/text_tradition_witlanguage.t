#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use Test::Warn;
use TryCatch;
use_ok( 'Text::Tradition' ); # with Language
use_ok( 'Text::Tradition::Witness' ); # with WitLanguage
}




1;
