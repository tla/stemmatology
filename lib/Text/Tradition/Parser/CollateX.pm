package Text::Tradition::Parser::CollateX;

use strict;
use warnings;
use Text::Tradition::Parser::GraphML qw/ graphml_parse populate_witness_path /;

=head1 NAME

Text::Tradition::Parser::CollateX

=head1 SYNOPSIS

  use Text::Tradition;
  
  my $t_from_file = Text::Tradition->new( 
    'name' => 'my text',
    'input' => 'CollateX',
    'file' => '/path/to/collation.xml'
    );
    
  my $t_from_string = Text::Tradition->new( 
    'name' => 'my text',
    'input' => 'CollateX',
    'string' => $collation_xml,
    );

=head1 DESCRIPTION

Parser module for Text::Tradition, given a GraphML file from the
CollateX program that describes a collation graph.  For further
information on the GraphML format for text collation, see
http://gregor.middell.net/collatex/

=head1 METHODS

=head2 B<parse>

parse( $tradition, $init_options );

Takes an initialized Text::Tradition object and a set of options; creates
the appropriate nodes and edges on the graph.  The options hash should
include either a 'file' argument or a 'string' argument, depending on the
source of the XML to be parsed.

=begin testing

use Text::Tradition;
binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";
eval { no warnings; binmode $DB::OUT, ":utf8"; };

my $cxfile = 't/data/Collatex-16.xml';
my $t = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'CollateX',
    'file'  => $cxfile,
    );

is( ref( $t ), 'Text::Tradition', "Parsed our own GraphML" );
if( $t ) {
    is( scalar $t->collation->readings, 26, "Collation has all readings" );
    is( scalar $t->collation->paths, 49, "Collation has all paths" );
    is( scalar $t->witnesses, 3, "Collation has all witnesses" );
    
    # Check an 'identical' node
    my $transposed = $t->collation->reading( 'n15' );
    ok( $transposed->has_primary, "Reading links to transposed primary" );
    is( $transposed->primary->name, 'n17', "Correct transposition link" );
}

=end testing

=cut

my $IDKEY = 'number';
my $CONTENTKEY = 'token';
my $TRANSKEY = 'identical';

sub parse {
    my( $tradition, $opts ) = @_;
    my $graph_data = graphml_parse( $opts );
    my $collation = $tradition->collation;
    my %witnesses; # Keep track of the witnesses we encounter as we
                   # run through the graph data.

    # Add the nodes to the graph.  First delete the start node, because
    # GraphML graphs will have their own start nodes.
    $collation->del_reading( $collation->start() );
    $collation->del_reading( $collation->end() );

    my $extra_data = {}; # Keep track of info to be processed after all
                         # nodes have been created
    foreach my $n ( @{$graph_data->{'nodes'}} ) {
        my %node_data = %$n;
        my $nodeid = delete $node_data{$IDKEY};
        my $token = delete $node_data{$CONTENTKEY};
        unless( defined $nodeid && defined $token ) {
            warn "Did not find an ID or token for graph node, can't add it";
            next;
        }
        my $gnode = $collation->add_reading( $nodeid );
        $gnode->text( $token );

        # Whatever is left is extra info to be processed later.
        if( keys %node_data ) {
            $extra_data->{$nodeid} = \%node_data;
        }
    }
        
    # Now add the edges.
    foreach my $e ( @{$graph_data->{'edges'}} ) {
        my %edge_data = %$e;
        my $from = delete $edge_data{'source'};
        my $to = delete $edge_data{'target'};

        # In CollateX, we have a distinct witness data ID per witness,
        # so that we can have multiple witnesses per edge.  We want to
        # translate this to one witness per edge in our own
        # representation.
        foreach my $ekey ( keys %edge_data ) {
            my $wit = $edge_data{$ekey};
            # Create the witness object if it does not yet exist.
            unless( $witnesses{$wit} ) {
                $tradition->add_witness( 'sigil' => $wit );
                $witnesses{$wit} = 1;
            }
            $collation->add_path( $from->{$IDKEY}, $to->{$IDKEY}, $wit );
        }
    }

    # Process the extra node data if it exists.
    foreach my $nodeid ( keys %$extra_data ) {
        my $ed = $extra_data->{$nodeid};
        if( exists $ed->{$TRANSKEY} ) {
            
            my $tn_reading = $collation->reading( $nodeid );
            my $main_reading = $collation->reading( $ed->{$TRANSKEY} );
            if( $collation->linear ) {
                $tn_reading->set_identical( $main_reading );
            } else {
                $collation->merge_readings( $main_reading, $tn_reading );
            }
        } # else we don't have any other tags to process yet.
    }

    # Find the beginning and end nodes of the graph.  The beginning node
    # has no incoming edges; the end node has no outgoing edges.
    my( $begin_node, $end_node );
    foreach my $gnode ( $collation->readings() ) {
        # print STDERR "Checking node " . $gnode->name . "\n";
        my @outgoing = $gnode->outgoing();
        my @incoming = $gnode->incoming();

        unless( scalar @incoming ) {
            warn "Already have a beginning node" if $begin_node;
            $begin_node = $gnode;
            $collation->start( $gnode );
        }
        unless( scalar @outgoing ) {
            warn "Already have an ending node" if $end_node;
            $end_node = $gnode;
            $collation->end( $gnode );
        }
    }
    
    # Set the $witness->path arrays for each wit.
    populate_witness_path( $tradition );

    # Rank the readings.
    $collation->calculate_ranks();
}
    
=head1 BUGS / TODO

=over

=item * Make this into a stream parser with GraphML

=item * Use CollateX-calculated ranks instead of recalculating our own

=back

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews, aurum@cpan.org

=cut

1;
