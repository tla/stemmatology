#!/usr/bin/env perl

use inc::Module::Install;
author( 'Tara L Andrews <aurum@cpan.org>' );
license( 'perl' );
perl_version( '5.012' );
all_from( 'lib/Text/Tradition.pm' );
requires( 'Algorithm::Diff' );
requires( 'Bio::Phylo::IO' );
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
requires( 'Module::Load' );
requires( 'Moose' );
requires( 'Moose::Util::TypeConstraints' );
requires( 'StackTrace::Auto' );
requires( 'Text::CSV_XS' );
requires( 'Text::CSV::Simple' ); # TODO delete
requires( 'Throwable::X' );
requires( 'TryCatch' );
requires( 'XML::LibXML' );
requires( 'XML::LibXML::XPathContext' );
build_requires( 'Test::Warn' );
&WriteAll;
