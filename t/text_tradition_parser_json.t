#!/usr/bin/perl -w

use strict;
use Test::More 'no_plan';
$| = 1;



# =begin testing
{
use Text::Tradition;
binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";
eval { no warnings; binmode $DB::OUT, ":utf8"; };

use_ok( 'Text::Tradition::Parser::JSON' );

open( JSFILE, 't/data/cx16.json' );
binmode JSFILE, ':utf8';
my @lines = <JSFILE>;
close JSFILE;

my $t = Text::Tradition->new(
    'name' => 'json',
    'input' => 'JSON',
    'string' => join( '', @lines ),
);

is( ref( $t ), 'Text::Tradition', "Parsed a JSON alignment" );
if( $t ) {
    is( scalar $t->collation->readings, 26, "Collation has all readings" );
    is( scalar $t->collation->paths, 32, "Collation has all paths" );
    is( scalar $t->witnesses, 3, "Collation has all witnesses" );
}

my %seen_wits;
map { $seen_wits{$_} = 0 } qw/ A B C /;
# Check that we have the right witnesses
foreach my $wit ( $t->witnesses ) {
	$seen_wits{$wit->sigil} = 1;
}
is( scalar keys %seen_wits, 3, "No extra witnesses were made" );
foreach my $k ( keys %seen_wits ) {
	ok( $seen_wits{$k}, "Witness $k still exists" );
}

# Check that the witnesses have the right texts
foreach my $wit ( $t->witnesses ) {
	my $origtext = join( ' ', @{$wit->text} );
	my $graphtext = $t->collation->path_text( $wit->sigil );
	is( $graphtext, $origtext, "Collation matches original for witness " . $wit->sigil );
}
}




1;
