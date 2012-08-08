use lib 'lib';
use feature 'say';
use strict;
use warnings;
use Getopt::Long;
use Text::Tradition::Directory;
use TryCatch;

binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';
eval { no warnings; binmode $DB::OUT, ':utf8'; $DB::deep = 1000 };

my( $dbuser, $dbpass );
my $dsn = 'dbi:SQLite:dbname=stemmaweb/db/traditions.db';

GetOptions( 
	'dsn=s'    => \$dsn,
	'u|user=s' => \$dbuser,
	'p|pass=s' => \$dbpass,
	);

my $dbopts = { dsn => $dsn };
$dbopts->{extra_args}->{user} = $dbuser if $dbuser;
$dbopts->{extra_args}->{password} = $dbpass if $dbpass;

my $dir = Text::Tradition::Directory->new( $dbopts );

my $scope = $dir->new_scope();
my $lookfor = $ARGV[0] || '';
foreach my $tinfo ( $dir->traditionlist() ) {
	next unless $tinfo->{'name'} =~ /$lookfor/ || $tinfo->{'id'} eq $lookfor;
	my $tradition = $dir->lookup( $tinfo->{'id'} );
	# Try to piece together the majority text.
	my $c = $tradition->collation;
	my $curr = $c->start;
	my @text;
	while( $curr ne $c->end ) {
		my %witnumbers;
		foreach my $candidate( $c->sequence->successors( $curr ) ) {
			my $cobj = $c->reading( $candidate );
			next if $cobj->is_lacuna;
			my $witnum = scalar keys %{$c->sequence->get_edge_attributes( 
				$curr->id, $candidate )};
			$witnumbers{$witnum} = $cobj;
		}
		my @numbers = sort { $a <=> $b } keys %witnumbers;
		if( @numbers ) {
			$curr = $witnumbers{ pop @numbers };
			push( @text, $curr ) unless $curr->is_meta;
		} else {
			warn "Did not find non-lacuna successor to $curr";
			last;
		}
	}
	my $pathtext = '';
	my $last;
	foreach my $r ( @text ) {
		unless ( $r->join_prior || !$last || $last->join_next ) {
			$pathtext .= ' ';
		} 
		$pathtext .= $r->text;
		$last = $r;
	}
	say $pathtext;
}