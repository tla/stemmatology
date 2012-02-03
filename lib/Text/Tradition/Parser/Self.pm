package Text::Tradition::Parser::Self;

use strict;
use warnings;
use Text::Tradition::Parser::GraphML qw/ graphml_parse /;

=head1 NAME

Text::Tradition::Parser::GraphML

=head1 SYNOPSIS

  use Text::Tradition;
  
  my $t_from_file = Text::Tradition->new( 
    'name' => 'my text',
    'input' => 'Self',
    'file' => '/path/to/tradition.xml'
    );
    
  my $t_from_string = Text::Tradition->new( 
    'name' => 'my text',
    'input' => 'Self',
    'string' => $tradition_xml,
    );

=head1 DESCRIPTION

Parser module for Text::Tradition to read in its own GraphML output format.
GraphML is a relatively simple graph description language; a 'graph' element
can have 'node' and 'edge' elements, and each of these can have simple 'data'
elements for attributes to be saved.

The graph itself has attributes as in the Collation object:

=over

=item * linear 

=item * ac_label

=item * baselabel

=item * wit_list_separator

=back

The node objects have the following attributes:

=over

=item * name

=item * reading

=item * identical

=item * rank

=item * class

=back

The edge objects have the following attributes:

=over

=item * class

=item * witness (for 'path' class edges)

=item * extra   (for 'path' class edges)

=item * relationship    (for 'relationship' class edges)

=item * equal_rank      (for 'relationship' class edges)

=item * non_correctable (for 'relationship' class edges)

=item * non_independent (for 'relationship' class edges)

=back

=head1 METHODS

=head2 B<parse>

parse( $graph, $opts );

Takes an initialized Text::Tradition object and a set of options; creates
the appropriate nodes and edges on the graph.  The options hash should
include either a 'file' argument or a 'string' argument, depending on the
source of the XML to be parsed.

=begin testing

use Text::Tradition;
binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";
eval { no warnings; binmode $DB::OUT, ":utf8"; };

my $tradition = 't/data/florilegium_graphml.xml';
my $t = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'Self',
    'file'  => $tradition,
    );

is( ref( $t ), 'Text::Tradition', "Parsed our own GraphML" );
if( $t ) {
    is( scalar $t->collation->readings, 319, "Collation has all readings" );
    is( scalar $t->collation->paths, 376, "Collation has all paths" );
    is( scalar $t->witnesses, 13, "Collation has all witnesses" );
}

=end testing

=cut

my( $IDKEY, $TOKENKEY, $TRANSPOS_KEY, $RANK_KEY,
	$START_KEY, $END_KEY, $LACUNA_KEY, $COMMON_KEY,
	$SOURCE_KEY, $TARGET_KEY, $WITNESS_KEY, $EXTRA_KEY, $RELATIONSHIP_KEY,
	$SCOPE_KEY, $CORRECT_KEY, $INDEP_KEY )
    = qw/ id text identical rank 
    	  is_start is_end is_lacuna is_common
    	  source target witness extra relationship
    	  scope non_correctable non_independent /;

sub parse {
    my( $tradition, $opts ) = @_;
    
    # Collation data is in the first graph; relationship-specific stuff 
    # is in the second.
    my( $graph_data, $rel_data ) = graphml_parse( $opts );
    
    my $collation = $tradition->collation;
    my %witnesses;
    
    # print STDERR "Setting graph globals\n";
    $tradition->name( $graph_data->{'name'} );
    my $use_version;
    foreach my $gkey ( keys %{$graph_data->{'global'}} ) {
		my $val = $graph_data->{'global'}->{$gkey};
		if( $gkey eq 'version' ) {
			$use_version = $val;
		} else {
			$collation->$gkey( $val );
		}
	}
		
    # Add the nodes to the graph. 

    # print STDERR "Adding graph nodes\n";
    foreach my $n ( @{$graph_data->{'nodes'}} ) {    	
    	# If it is the start or end node, we already have one, so
    	# grab the rank and go.
    	next if( defined $n->{$START_KEY} );
    	if( defined $n->{$END_KEY} ) {
    		$collation->end->rank( $n->{$RANK_KEY} );
    		next;
    	}
    	
    	# First extract the data that we can use without reference to
    	# anything else.
        
        # Create the node.  
        my $reading_options = { 
        	'id' => $n->{$IDKEY},
        	'is_lacuna' => $n->{$LACUNA_KEY},
        	'is_common' => $n->{$COMMON_KEY},
        	};
        my $rank = $n->{$RANK_KEY};
		$reading_options->{'rank'} = $rank if $rank;
		my $text = $n->{$TOKENKEY};
		$reading_options->{'text'} = $text if $text;

		my $gnode = $collation->add_reading( $reading_options );
    }
        
    # Now add the edges.
    # print STDERR "Adding graph edges\n";
    foreach my $e ( @{$graph_data->{'edges'}} ) {
        my $from = $e->{$SOURCE_KEY};
        my $to = $e->{$TARGET_KEY};

		# We need the witness, and whether it is an 'extra' reading path.
		my $wit = $e->{$WITNESS_KEY};
		warn "No witness label on path edge!" unless $wit;
		my $extra = $e->{$EXTRA_KEY};
		my $label = $wit . ( $extra ? $collation->ac_label : '' );
		$collation->add_path( $from->{$IDKEY}, $to->{$IDKEY}, $label );
		# Add the witness if we don't have it already.
		unless( $witnesses{$wit} ) {
			$tradition->add_witness( sigil => $wit );
			$witnesses{$wit} = 1;
		}
		$tradition->witness( $wit )->is_layered( 1 ) if $extra;
    }
    
    ## Done with the main graph, now look at the relationships.
	# Nodes are added via the call to add_reading above.  We only need
	# add the relationships themselves.
	# TODO check that scoping does trt
	foreach my $e ( @{$rel_data->{'edges'}} ) {
		my $from = $e->{$SOURCE_KEY};
		my $to = $e->{$TARGET_KEY};
		my $relationship_opts = {
			'type' => $e->{$RELATIONSHIP_KEY},
			'scope' => $e->{$SCOPE_KEY},
			};
		$relationship_opts->{'non_correctable'} = $e->{$CORRECT_KEY}
			if exists $e->{$CORRECT_KEY};
		$relationship_opts->{'non_independent'} = $e->{$INDEP_KEY}
			if exists $e->{$INDEP_KEY};
		$collation->add_relationship( $from->{$IDKEY}, $to->{$IDKEY}, 
			$relationship_opts );
	}
	
    # Save the text for each witness so that we can ensure consistency
    # later on
	$tradition->collation->text_from_paths();	

}

1;

=head1 BUGS / TODO

=over

=item * Make this into a stream parser with GraphML

=item * Simply field -> attribute correspondence for nodes and edges

=item * Share key name constants with Collation.pm

=back

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews E<lt>aurum@cpan.orgE<gt>
