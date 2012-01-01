#!/usr/bin/env perl

use inc::Module::Install;
author( 'Tara L Andrews <aurum@cpan.org>' );
license( 'perl' );
perl_version( '5.012' );
all_from( 'lib/Text/Tradition.pm' );
requires( 'Algorithm::Diff' );
requires( 'Bio::Phylo::IO' );
requires( 'File::chdir' );
requires( 'Graph' );
requires( 'Graph::Reader::Dot' );
requires( 'IPC::Run' );
requires( 'KiokuDB::Backend::DBI' );
requires( 'KiokuDB::TypeMap' );
requires( 'KiokuDB::TypeMap::Entry::Naive' );
requires( 'KiokuX::Model' );
requires( 'Module::Load' );
requires( 'Moose' );
requires( 'Moose::Util::TypeConstraints' );
requires( 'Text::CSV_XS' );
requires( 'Text::CSV::Simple' ); # TODO delete
requires( 'XML::LibXML' );
requires( 'XML::LibXML::XPathContext' );
build_requires( 'Test::Warn' );
&WriteAll;
