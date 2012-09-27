#!/usr/bin/env perl

use lib 'lib';
use strict;
use warnings;
use Getopt::Long;
use TryCatch;
use Text::Tradition;
use Text::Tradition::Directory;
use Text::Tradition::StemmaUtil qw/ character_input phylip_pars /;

binmode STDERR, ":utf8";
binmode STDOUT, ":utf8";
eval { no warnings; binmode $DB::OUT, ":utf8"; };

# Variables with defaults
my( $informat, $outformat, $language, $name, $sep, $dsn )  = ( '', '', 'Default', 
	'Tradition', "\t", "dbi:SQLite:dbname=db/traditions.db" );
# Variables with no default
my( $inbase, $help, $stemmafile,  $dbuser, $dbpass, $from, $to, $dbid, $debug, $nonlinear );

GetOptions( 'i|in=s'    => \$informat,
            'b|base=s'  => \$inbase,
            'o|out=s'   => \$outformat,
            'l|language=s' => \$language,
            'n|name=s'  => \$name,
            'h|help'    => \$help,
            's|stemma=s' => \$stemmafile,
            'u|user=s'  => \$dbuser,
            'p|pass=s'  => \$dbpass,
            'f|from=s'  => \$from,
            't|to=s'    => \$to,
            'nl|nonlinear' => \$nonlinear,
            'sep=s'		=> \$sep,
            'dsn=s'		=> \$dsn,
	    'dbid=s'    => \$dbid,
	    	'debug'     => \$debug
    );

if( $help ) {
    help();
}

unless( $informat =~ /^(CSV|CTE|KUL|Self|TEI|CollateX|tab(ular)?)|stone|db$/i ) {
    help( "Input format must be one of CollateX, CSV, CTE, Self, TEI" );
}
$informat = 'CollateX' if $informat =~ /^c(ollate)?x$/i;
$informat = 'KUL' if $informat =~ /^kul$/i;
$informat = 'CTE' if $informat =~ /^cte$/i;
$informat = 'Self' if $informat =~ /^self$/i;
$informat = 'TEI' if $informat =~ /^tei$/i;
$informat = 'Tabular' if $informat =~ /^tab$/i;
$informat = 'CollateText' if $informat =~ /^stone$/i;

unless( $outformat =~ /^(graphml|svg|dot|stemma|csv|db)$/ ) {
    help( "Output format must be one of db, graphml, svg, csv, stemma, or dot" );
}

if( $from || $to ) {
	help( "Subgraphs only supported in GraphML, dot, or SVG format" ) 
		unless $outformat =~ /^(graphml|dot|svg)$/;
}

# Do we have a base if we need it?
if( $informat =~ /^(KUL|CollateText)$/ && !$inbase ) {
    help( "$informat input needs a base text" );
}
$sep = "\t" if $sep eq 'tab';

my $input = $ARGV[0];
my $tradition;
my $dir;
if( $informat eq 'db' ) {
	my $dbargs = { dsn => $dsn };
	$dbargs->{'extra_args'}->{'user'} = $dbuser if $dbuser;
	$dbargs->{'extra_args'}->{'password'} = $dbpass if $dbpass;
	$dir = Text::Tradition::Directory->new( $dbargs );
	my $scope = $dir->new_scope();
	$tradition = $dir->lookup( $input );
} else {
	# First: read the base. Make a graph, but also note which
	# nodes represent line beginnings.
	my %args = ( 'input' => $informat,
				 'file' => $input );
	$args{'linear'} = 0 if $nonlinear;
	$args{'base'} = $inbase if $inbase;
	$args{'language'} = $language if $language;
	$args{'name'} = $name if $name;
	$args{'sep_char'} = $sep if $informat eq 'Tabular';
	### Custom hacking for Stone
	if( $informat eq 'CollateText' ) {
		$args{'sigla'} = [ qw/ S M X V Z Bb B K W L / ];
	}
	$tradition = Text::Tradition->new( %args );
}
if( $stemmafile ) {
	my $stemma = $tradition->add_stemma( dotfile => $stemmafile );
	print STDERR "Saved stemma at $stemmafile\n" if $stemma;
}

# Now output what we have been asked to.
if( $outformat eq 'stemma' ) {
    my $cdata = character_input( $tradition->collation->alignment_table );
    try {
    	print phylip_pars( $cdata );
    } catch( Text::Tradition::Error $e ) {
        print STDERR "Bad result: " . $e->message;
    }
} elsif( $outformat eq 'db' ) {
	unless( $dir ) {
		my $extra_args = { 'create' => 1 };
		$extra_args->{'user'} = $dbuser if $dbuser;
		$extra_args->{'password'} = $dbpass if $dbpass;
		$dir = Text::Tradition::Directory->new( 'dsn' => $dsn, 
			'extra_args' => $extra_args );
	}
	my $scope = $dir->new_scope;
	my $uuid;
	if( $dbid ) {
		$uuid = $dir->store( $dbid => $tradition );
	} else {
		$uuid = $dir->store( $tradition );
	}
	print STDERR "Saved tradition to database with ID $uuid\n";
} else {
    my $output = "as_$outformat";
    my $opts = {};
    $opts->{'from'} = $from if $from;
    $opts->{'to'} = $to if $to;
    $opts->{'nocalc'} = 1 if $debug;
    print $tradition->collation->$output( $opts );
}

sub help {
    my( $msg ) = @_;
    print STDERR << "EOF"
Usage: $0 -i [format] -o [format] (--base [filename]) (--(no)linear) [inputfile]
    i, input: Format of the input file.  Must be one of CollateX, CSV, CTE, Self, TEI.
    o, output: Format of the output.  Must be one of svg, dot, graphml, csv, stemma.
    b, base: Filename that contains a base text.  Needed for CSV input.
    l, linear: Treat transposed readings separately, producing a linear graph.  
        If nolinear, treat transposed readings as the same node.
    h, help: Print this message.
EOF
    ;
    if( $msg ) {
        print STDERR "$msg\n";
    }
    exit ($msg ? 1 : 0 );
}
