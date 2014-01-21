package Text::Tradition::Datatypes;

use Moose::Util::TypeConstraints;
use XML::Easy::Syntax qw( $xml10_name_rx );

enum 'Ternary' => [ qw( yes maybe no ) ];

enum 'RelationshipScope' => [ qw( local document global ) ];

subtype 'ReadingID',
	as 'Str',
	where { $_ =~ /\A$xml10_name_rx\z/ },
	message { 'Reading ID must be a valid XML attribute string' };
	
subtype 'SourceType',
	as 'Str',
	where { $_ =~ /^(xmldesc|plaintext|json|collation)$/ },
	message { 'Source type must be one of xmldesc, plaintext, json, collation' };
	
subtype 'Sigil',
	as 'Str',
	where { $_ =~ /\A$xml10_name_rx\z/ },
	message { 'Sigil must be a valid XML attribute string' };

1;