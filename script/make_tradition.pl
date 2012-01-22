#!/usr/bin/env perl

use lib 'lib';
use strict;
use warnings;
use Getopt::Long;
use Text::Tradition;
use Text::Tradition::Directory;
use Text::Tradition::StemmaUtil;

binmode STDERR, ":utf8";
binmode STDOUT, ":utf8";
eval { no warnings; binmode $DB::OUT, ":utf8"; };

my( $informat, $inbase, $outformat, $help, $linear, $name, $HACK, $sep, $stemmafile, $dsn ) 
    = ( '', '', '', '', 1, 'Tradition', 0, "\t", '',
    	"dbi:SQLite:dbname=stemmaweb/db/traditions.db" );

GetOptions( 'i|in=s'    => \$informat,
            'b|base=s'  => \$inbase,
            'o|out=s'   => \$outformat,
            'l|linear!' => \$linear,
            'n|name=s'  => \$name,
            'h|help'    => \$help,
            's|stemma=s' => \$stemmafile,
            'sep=s'		=> \$sep,
            'hack'      => \$HACK,
            'dsn=s'		=> \$dsn,
    );

if( $help ) {
    help();
}

unless( $informat =~ /^(CSV|CTE|KUL|Self|TEI|CollateX|tab(ular)?)|stone$/i ) {
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

# Do we have a base if we need it?
if( $informat =~ /^(KUL|CollateText)$/ && !$inbase ) {
    help( "$informat input needs a base text" );
}
$sep = "\t" if $sep eq 'tab';

my $input = $ARGV[0];

# First: read the base. Make a graph, but also note which
# nodes represent line beginnings.
my %args = ( 'input' => $informat,
             'file' => $input,
             'linear' => $linear );
$args{'base'} = $inbase if $inbase;
$args{'name'} = $name if $name;
$args{'sep_char'} = $sep if $informat eq 'Tabular';
### Custom hacking for Stone
if( $informat eq 'CollateText' ) {
    $args{'sigla'} = [ qw/ S M X V Z Bb B K W L / ];
}
my $tradition = Text::Tradition->new( %args );
if( $stemmafile ) {
	my $stemma = $tradition->add_stemma( dotfile => $stemmafile );
	print STDERR "Saved stemma at $stemmafile\n" if $stemma;
}

### Custom hacking
# Remove witnesses C, E, G in the Matthew text
if( $HACK ) {
	my @togo = qw/ C E G /;
	$tradition->collation->clear_witness( @togo );
	$tradition->del_witness( @togo );
}

# Now output what we have been asked to.
if( $outformat eq 'stemma' ) {
    my $cdata = character_input( $tradition->collation->make_alignment_table );
    my( $result, $tree ) = phylip_pars( $cdata );
    if( $result ) {
        print $tree;
    } else {
        print STDERR "Bad result: $tree";
    }
} elsif( $outformat eq 'db' ) {
	my $dir = Text::Tradition::Directory->new( 'dsn' => $dsn, 
		'extra_args' => { 'create' => 1 } );
	my $scope = $dir->new_scope;
	my $uuid = $dir->store( $tradition );
	print STDERR "Saved tradition to database with ID $uuid\n";
} else {
    my $output = "as_$outformat";
    print $tradition->collation->$output();
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
