#!/usr/bin/env perl

use inc::Module::Install;
author( 'Tara L Andrews <aurum@cpan.org>' );
license( 'perl' );
perl_version( '5.012' );
all_from( 'lib/Text/Tradition/Morphology.pm' );
requires( 'IPC::Run' );
requires( 'JSON' );
requires( 'Lingua::Features' => '0.3.2' );
requires( 'Lingua::Features::Structure' );
requires( 'Lingua::TagSet' => '0.3.3' );
requires( 'Lingua::TagSet::Multext' );
requires( 'Lingua::TagSet::TreeTagger::French' );
requires( 'Lingua::TagSet::TreeTagger::English' );
requires( 'Module::Load' );
requires( 'Moose' );
requires( 'Moose::Role' );
requires( 'Text::Tradition' => '2.1' );
requires( 'TryCatch' );
build_requires( 'Safe::Isa' );
build_requires( 'Test::Warn' );

# Modules needed for morphology but not trivially CPANnable
recommends( 'Lingua::TreeTagger' );
recommends( 'Flemm' );
&WriteAll;
