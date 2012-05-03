use lib 'lib';
use strict;
use warnings;
use Test::More;
use Text::Tradition;

eval "use Flemm";
plan skip_all => "Flemm 3.1 required" if $@;

binmode( STDOUT, ':utf8' );
binmode( STDERR, ':utf8' );

my $tf = Text::Tradition->new(
	'input' => 'Self',
	'file' => 't/data/besoin.xml',
	'language' => 'French' );
	
$tf->lemmatize();
my $graphmlstr = $tf->collation->as_graphml;
like( $graphmlstr, qr/graphml xmlns/, 
	"Serialized tradition after lemmatization" );

my $tf2 = Text::Tradition->new(
	input => 'Self',
	string => $graphmlstr,
	language => 'French' );

is( ref $tf2, 'Text::Tradition', "Re-parsed tradition with lemmatization" );
is( $tf->name, $tf2->name, "Traditions have same name" );
foreach my $r ( $tf->collation->readings ) {
	my $r2 = $tf2->collation->reading( $r->id );
	is( ref $r2, 'Text::Tradition::Collation::Reading',
		"Reading $r exists in new tradition" );
	if( $r2 ) {
		is( scalar $r->lexemes, scalar $r2->lexemes,
			"Same number of lexemes in new tradition for $r" );
	}
}

done_testing();
