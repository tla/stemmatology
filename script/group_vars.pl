#!/usr/bin/env perl

use lib 'lib';
use strict;
use warnings;
use JSON;
use Text::Tradition;
use Text::Tradition::Analysis qw/ group_variants /;
use Text::Tradition::Stemma;

binmode STDERR, ":utf8";
binmode STDOUT, ":utf8";
eval { no warnings; binmode $DB::OUT, ":utf8"; };

# Parse the tradition data
my $informat = 'Self';

my %args = ( 'input' => $informat,
             'file'  => $ARGV[0] );
my $tradition = Text::Tradition->new( %args );

# Parse the stemma data
my $stemma = Text::Tradition::Stemma->new( 'dot' => $ARGV[1] );

my $wits = {};
map { $wits->{$_} = 1 } $stemma->witnesses;

my $variant_groups = group_variants( $tradition->collation, $wits );

print encode_json( $variant_groups );