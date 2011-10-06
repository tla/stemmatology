#!/usr/bin/env perl

use lib 'lib';
use strict;
use warnings;
use Getopt::Long;
use Text::Tradition;
use Text::Tradition::Stemma;

binmode STDERR, ":utf8";
binmode STDOUT, ":utf8";
eval { no warnings; binmode $DB::OUT, ":utf8"; };

my( $informat, $inbase, $outformat, $help, $linear, $name, $HACK ) 
    = ( '', '', '', '', 0, 0 );

GetOptions( 'i|in=s'    => \$informat,
            'b|base=s'  => \$inbase,
            'o|out=s'   => \$outformat,
            'l|linear!' => \$linear,
            'n|name'    => \$name,
            'h|help'    => \$help,
            'hack'      => \$HACK,
    );

if( $help ) {
    help();
}

unless( $informat =~ /^(CSV|CTE|KUL|Self|TEI|CollateX|tab(ular)?)$/i ) {
    help( "Input format must be one of CollateX, CSV, CTE, Self, TEI" );
}
$informat = 'CollateX' if $informat =~ /^c(ollate)?x$/i;
$informat = 'KUL' if $informat =~ /^kul$/i;
$informat = 'CTE' if $informat =~ /^cte$/i;
$informat = 'Self' if $informat =~ /^self$/i;
$informat = 'TEI' if $informat =~ /^tei$/i;
$informat = 'Tabular' if $informat =~ /^tab$/i;

unless( $outformat =~ /^(graphml|svg|dot|stemma|csv)$/ ) {
    help( "Output format must be one of graphml, svg, csv, stemma, or dot" );
}

# Do we have a base if we need it?
if( $informat eq 'KUL' && !$inbase ) {
    help( "$informat input needs a base text" );
}

my $input = $ARGV[0];

# First: read the base. Make a graph, but also note which
# nodes represent line beginnings.
my %args = ( 'input' => $informat,
             'file' => $input,
             'linear' => $linear );
$args{'base'} = $inbase if $inbase;
$args{'name'} = $name if $name;
my $tradition = Text::Tradition->new( %args );

### Custom hacking
# Remove witnesses C, E, G in the Matthew text
if( $HACK ) {
    foreach( $tradition->collation->paths() ) {
        $tradition->collation->del_path( $_ ) if $_->label =~ /^[ceg]$/i;
    }
    foreach( $tradition->collation->readings() ) {
        if( !$_->outgoing() && !$_->incoming() ) {
            print STDERR "Deleting reading " . $_->label . "\n";
            $tradition->collation->del_reading( $_ );
        }
    }
}

# Now output what we have been asked to.
if( $outformat eq 'stemma' ) {
    my $stemma = Text::Tradition::Stemma->new( 
        'collation' => $tradition->collation );
    my( $result, $tree ) = $stemma->run_phylip_pars();
    if( $result ) {
        print $tree;
    } else {
        print STDERR "Bad result: $tree";
    }
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
