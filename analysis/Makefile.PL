#!/usr/bin/env perl

use inc::Module::Install;
author( 'Tara L Andrews <aurum@cpan.org>' );
license( 'perl' );
perl_version( '5.012' );
all_from( 'lib/Text/Tradition/Analysis.pm' );
requires( 'Algorithm::Diff' );
requires( 'Bio::Phylo::IO' );
requires( 'File::chdir' );
requires( 'File::Which' );
requires( 'Graph' );
requires( 'Graph::Reader::Dot' );
requires( 'IPC::Run' );
requires( 'JSON' );
requires( 'LWP::UserAgent' );
requires( 'Moose' );
requires( 'Moose::Role' );
requires( 'Set::Scalar' );
requires( 'Text::Tradition' => '1.1' );
requires( 'Text::Tradition::Error' );
requires( 'TryCatch' );

build_requires( 'Test::More::UTF8' );
build_requires( 'Text::Tradition::Directory' );
&WriteAll;
