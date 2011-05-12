#!/usr/bin/env perl

use 5.010;
use inc::Module::Install;
author( 'Tara L Andrews <aurum@cpan.org>' );
license( 'perl' );
all_from( 'lib/Text/Tradition' );
requires( 'perl' => '5.010' );
requires( 'XML::LibXML' );
requires( 'Graph::Easy' );
requires( 'Text::CSV::Simple' );
&WriteAll;
