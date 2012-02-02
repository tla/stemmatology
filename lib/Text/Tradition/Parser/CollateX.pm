package Text::Tradition::Parser::CollateX;

use strict;
use warnings;
use Text::Tradition::Parser::GraphML qw/ graphml_parse /;

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
    is( scalar $t->collation->paths, 32, "Collation has all paths" );
    is( scalar $t->witnesses, 3, "Collation has all witnesses" );
    
    # Check an 'identical' node
    my $transposed = $t->collation->reading( 'n15' );
    my @related = $transposed->related_readings;
    is( scalar @related, 1, "Reading links to transposed version" );
    is( $related[0]->id, 'n17', "Correct transposition link" );
}

=end testing

=cut

my $IDKEY = 'number';
my $CONTENTKEY = 'token';
my $TRANSKEY = 'identical';

sub parse {
    my( $tradition, $opts ) = @_;
    my( $graph_data ) = graphml_parse( $opts );
    my $collation = $tradition->collation;

	# First add the readings to the graph.
    my $extra_data = {}; # Keep track of info to be processed after all
                         # nodes have been created
    foreach my $n ( @{$graph_data->{'nodes'}} ) {
        unless( defined $n->{$IDKEY} && defined $n->{$CONTENTKEY} ) {
            warn "Did not find an ID or token for graph node, can't add it";
            next;
        }
        my %node_data = %$n;
        my $gnode_args = { 
        	'id' => delete $node_data{$IDKEY},
        	'text' => delete $node_data{$CONTENTKEY},
        };
        my $gnode = $collation->add_reading( $gnode_args );

        # Whatever is left is extra info to be processed later,
        # e.g. a transposition link.
        if( keys %node_data ) {
            $extra_data->{$gnode->id} = \%node_data;
        }
    }
        
    # Now add the path edges.
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
            unless( $tradition->witness( $wit ) ) {
                $tradition->add_witness( 'sigil' => $wit );
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
                $collation->add_relationship( $tn_reading, $main_reading,
                	{ type => 'transposition' } );
            } else {
                $collation->merge_readings( $main_reading, $tn_reading );
            }
        } # else we don't have any other tags to process yet.
    }

    # Find the beginning and end nodes of the graph.  The beginning node
    # has no incoming edges; the end node has no outgoing edges.
    my( $begin_node, $end_node );
    my @starts = $collation->sequence->source_vertices();
    my @ends = $collation->sequence->sink_vertices();
    if( @starts != 1 ) {
    	warn "Found more or less than one start vertex: @starts";
    } else {
    	$collation->merge_readings( $collation->start, @starts );
    }
    if( @ends != 1 )  {
    	warn "Found more or less than one end vertex: @ends";
    } else {
    	$collation->merge_readings( $collation->end, @ends );
    }
    
    # Rank the readings.
    $collation->calculate_common_readings(); # will implicitly rank

    # Save the text for each witness so that we can ensure consistency
    # later on
	$tradition->collation->text_from_paths();	
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
