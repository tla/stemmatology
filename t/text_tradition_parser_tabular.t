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

my $csv = 't/data/florilegium.csv';
my $t = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'Tabular',
    'file'  => $csv,
    'sep_char' => ',',
    );

is( ref( $t ), 'Text::Tradition', "Parsed florilegium CSV file" );

### TODO Check these figures
if( $t ) {
    is( scalar $t->collation->readings, 311, "Collation has all readings" );
    is( scalar $t->collation->paths, 361, "Collation has all paths" );
    is( scalar $t->witnesses, 13, "Collation has all witnesses" );
}

# Check that we have the right witnesses
my %seen_wits;
map { $seen_wits{$_} = 0 } qw/ A B C D E F G H K P Q S T /;
foreach my $wit ( $t->witnesses ) {
	$seen_wits{$wit->sigil} = 1;
}
is( scalar keys %seen_wits, 13, "No extra witnesses were made" );
foreach my $k ( keys %seen_wits ) {
	ok( $seen_wits{$k}, "Witness $k still exists" );
}

# Check that the witnesses have the right texts
foreach my $wit ( $t->witnesses ) {
	my $origtext = join( ' ', @{$wit->text} );
	my $graphtext = $t->collation->path_text( $wit->sigil );
	is( $graphtext, $origtext, "Collation matches original for witness " . $wit->sigil );
}

# Check that the a.c. witnesses have the right text
map { $seen_wits{$_} = 0 } qw/ A B C D F G H K S /;
foreach my $k ( keys %seen_wits ) {
	my $wit = $t->witness( $k );
	if( $seen_wits{$k} ) {
		ok( $wit->is_layered, "Witness $k got marked as layered" );
		ok( $wit->has_layertext, "Witness $k has an a.c. version" );
		my $origtext = join( ' ', @{$wit->layertext} );
		my $acsig = $wit->sigil . $t->collation->ac_label;
		my $graphtext = $t->collation->path_text( $acsig );
		is( $graphtext, $origtext, "Collation matches original a.c. for witness $k" );
	} else {
		ok( !$wit->is_layered, "Witness $k not marked as layered" );
		ok( !$wit->has_layertext, "Witness $k has no a.c. version" );
	}
}	

# Check that we only have collation relationships where we need them
is( scalar $t->collation->relationships, 3, "Redundant collations were removed" );
foreach my $rel ( $t->collation->relationships ) {
	print STDERR $rel->[0] . " -> " . $rel->[1] . "\n";
}
}




1;
