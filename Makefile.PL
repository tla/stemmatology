#!/usr/bin/env perl

use inc::Module::Install;
author( 'Tara L Andrews <aurum@cpan.org>' );
license( 'perl' );
perl_version( '5.012' );
all_from( 'lib/Text/Tradition.pm' );
requires( 'Algorithm::Diff' );
requires( 'Bio::Phylo::IO' );
requires( 'Class::Load' );
requires( 'DBI' );
requires( 'File::chdir' );
requires( 'File::Which' );
requires( 'Graph' );
requires( 'Graph::Reader::Dot' );
requires( 'IPC::Run' );
requires( 'JSON' );
requires( 'KiokuDB::Backend::DBI' );
requires( 'KiokuDB::GC::Naive' );
requires( 'KiokuDB::TypeMap' );
requires( 'KiokuDB::TypeMap::Entry::Naive' );
requires( 'KiokuX::Model' );
requires( 'KiokuX::User::Util' );
requires( 'Module::Load' );
requires( 'Moose' );
requires( 'Moose::Util::TypeConstraints' );
requires( 'StackTrace::Auto' );
requires( 'Text::CSV' );
requires( 'Text::TEI::Markup' => '1.7' );
requires( 'Throwable::X' );
requires( 'TryCatch' );
requires( 'XML::Easy::Syntax' );
requires( 'XML::LibXML' );
requires( 'XML::LibXML::XPathContext' );
requires( 'YAML::XS' );
requires( 'namespace::clean' );
# For the morphology stuff
requires( 'Lingua::TagSet::Multext' );
requires( 'Lingua::TagSet::TreeTagger::French' );
requires( 'Lingua::TagSet::TreeTagger::English' );
requires( 'Lingua::Features::Structure' );
build_requires( 'Data::Dump' );
build_requires( 'Test::Warn' );
build_requires( 'Test::Memory::Cycle' );
# Modules needed for morphology but not trivially CPANnable
recommends( 'Lingua::TreeTagger' );
recommends( 'Flemm' );
&WriteAll;
