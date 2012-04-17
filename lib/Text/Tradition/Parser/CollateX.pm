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

is( ref( $t ), 'Text::Tradition', "Parsed a CollateX input" );
if( $t ) {
    is( scalar $t->collation->readings, 26, "Collation has all readings" );
    is( scalar $t->collation->paths, 32, "Collation has all paths" );
    is( scalar $t->witnesses, 3, "Collation has all witnesses" );
    
    # Check an 'identical' node
    my $transposed = $t->collation->reading( 'n15' );
    my @related = $transposed->related_readings;
    is( scalar @related, 1, "Reading links to transposed version" );
    is( $related[0]->id, 'n18', "Correct transposition link" );
}

=end testing

=cut

my $IDKEY = 'number';
my $CONTENTKEY = 'tokens';
my $EDGETYPEKEY = 'type';
my $WITKEY = 'witnesses';

sub parse {
    my( $tradition, $opts ) = @_;
    my( $graph_data ) = graphml_parse( $opts );
    my $collation = $tradition->collation;

	# First add the readings to the graph.
	## Assume the start node has no text and id 0, and the end node has
	## no text and ID [number of nodes] - 1.
    my $endnode = scalar @{$graph_data->{'nodes'}} - 1;
    foreach my $n ( @{$graph_data->{'nodes'}} ) {
        unless( defined $n->{$IDKEY} && defined $n->{$CONTENTKEY} ) {
        	if( defined $n->{$IDKEY} && $n->{$IDKEY} == 0 ) {
        		# It's the start node.
        		$n->{$IDKEY} = $collation->start->id;
        	} elsif ( defined $n->{$IDKEY} && $n->{$IDKEY} == $endnode ) {
        		# It's the end node.
        		$n->{$IDKEY} = $collation->end->id;
        	} else {
        		# Something is probably wrong.
				warn "Did not find an ID or token for graph node, can't add it";
        	} 
            next;
        }
        # Node ID should be an XML name, so prepend an 'n' if necessary.
        if( $n->{$IDKEY} =~ /^\d/ ) {
			$n->{$IDKEY} = 'n' . $n->{$IDKEY};
		}
		# Create the reading.
        my $gnode_args = { 
        	'id' => $n->{$IDKEY},
        	'text' => $n->{$CONTENTKEY},
        };
        my $gnode = $collation->add_reading( $gnode_args );
    }
        
    # Now add the path edges.
    foreach my $e ( @{$graph_data->{'edges'}} ) {
        my $from = $e->{'source'};
        my $to = $e->{'target'};
        
        ## Edge data keys are ID (which we don't need), witnesses, and type.
        ## Type can be 'path' or 'relationship'; 
        ## witnesses is a comma-separated list.
		if( $e->{$EDGETYPEKEY} eq 'path' ) {
			## Add the path for each witness listesd.
            # Create the witness objects if they does not yet exist.
            foreach my $wit ( split( /, /, $e->{$WITKEY} ) ) {
				unless( $tradition->witness( $wit ) ) {
					$tradition->add_witness( 
						'sigil' => $wit, 'sourcetype' => 'collation' );
				}
				$collation->add_path( $from->{$IDKEY}, $to->{$IDKEY}, $wit );
			}
        } else { # type 'relationship'
        	$collation->add_relationship( $from->{$IDKEY}, $to->{$IDKEY},
        		{ 'type' => 'transposition' } );
        }
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
