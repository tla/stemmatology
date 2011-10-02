#!/usr/bin/env perl

use 5.010;
use inc::Module::Install;
author( 'Tara L Andrews <aurum@cpan.org>' );
license( 'perl' );
all_from( 'lib/Text/Tradition' );
requires( 'perl' => '5.010' );
requires( 'Algorithm::Diff' );
requires( 'Bio::Phylo::IO' );
requires( 'File::chdir' );
requires( 'Graph' );
requires( 'Graph::Convert' );
requires( 'Graph::Easy' );
requires( 'Graph::Reader::Dot' );
requires( 'IPC::Run' );
requires( 'Module::Load' );
requires( 'Moose' );
requires( 'Moose::Util::TypeConstraints' );
requires( 'MooseX::NonMoose' );
# TODO get rid of this dependency
requires( 'Text::CSV::Simple' );
requires( 'Text::CSV_XS' );
requires( 'XML::LibXML' );
requires( 'XML::LibXML::XPathContext' );
&WriteAll;
