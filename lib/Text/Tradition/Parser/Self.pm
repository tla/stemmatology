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

my( $IDKEY, $TOKENKEY, $TRANSPOS_KEY, $RANK_KEY, $CLASS_KEY,
	$START_KEY, $END_KEY, $LACUNA_KEY,
	$SOURCE_KEY, $TARGET_KEY, $WITNESS_KEY, $EXTRA_KEY, $RELATIONSHIP_KEY,
	$COLO_KEY, $CORRECT_KEY, $INDEP_KEY )
    = qw/ name reading identical rank class
    	  is_start is_end is_lacuna 
    	  source target witness extra relationship
    	  equal_rank non_correctable non_independent /;

sub parse {
    my( $tradition, $opts ) = @_;
    my $graph_data = graphml_parse( $opts );
    
    my $collation = $tradition->collation;
    my %witnesses;
    
    # Set up the graph-global attributes.  They will appear in the
    # hash under their accessor names.
    my $use_version;
    print STDERR "Setting graph globals\n";
    $tradition->name( $graph_data->{'name'} );
	$DB::single = 1;
    foreach my $gkey ( keys %{$graph_data->{'global'}} ) {
		my $val = $graph_data->{'global'}->{$gkey};
		if( $gkey eq 'version' ) {
			$use_version = $val;
		} else {
			$collation->$gkey( $val );
		}
	}
	if( $use_version ) {
		# Many of our tags changed.
		$IDKEY = 'id';
		$TOKENKEY = 'text';
		$COLO_KEY = 'colocated';
	}
		
    # Add the nodes to the graph. 

    my $extra_data = {}; # Keep track of data that needs to be processed
                         # after the nodes & edges are created.
    print STDERR "Adding graph nodes\n";
    foreach my $n ( @{$graph_data->{'nodes'}} ) {
    	# If it is the start or end node, we already have one, so skip it.
    	next if defined $n->{$START_KEY} || defined $n->{$END_KEY};
    	
    	# First extract the data that we can use without reference to
    	# anything else.
    	my %node_data = %$n; # Need $n itself untouched for edge processing
        
        # Create the node.  
        my $reading_options = { 
        	'id' => delete $node_data{$IDKEY},
        	'is_lacuna' => delete $node_data{$LACUNA_KEY},
        	};
        my $rank = delete $node_data{$RANK_KEY};
		$reading_options->{'rank'} = $rank if $rank;
		my $text = delete $node_data{$TOKENKEY};
		$reading_options->{'text'} = $text if $text;

        # This is a horrible hack for backwards compatibility.
        unless( $use_version ) {
			$reading_options->{'is_lacuna'} = 1 
				if $reading_options->{'text'} =~ /^\#LACUNA/;
		}
		
		delete $node_data{$CLASS_KEY}; # Not actually used
		my $gnode = $collation->add_reading( $reading_options );

        # Now save the data that we need for post-processing,
        # if it exists. TODO this is unneeded after conversion
        if ( keys %node_data ) {
            $extra_data->{$gnode->id} = \%node_data
        }
    }
        
    # Now add the edges.
    print STDERR "Adding graph edges\n";
    foreach my $e ( @{$graph_data->{'edges'}} ) {
        my $from = $e->{$SOURCE_KEY};
        my $to = $e->{$TARGET_KEY};
        my $class = $e->{$CLASS_KEY};

        # We may have more information depending on the class.
        if( $class eq 'path' ) {
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
        } elsif( $class eq 'relationship' ) {
        	# We need the metadata about the relationship.
        	my $opts = { 'type' => $e->{$RELATIONSHIP_KEY} };
        	$opts->{$COLO_KEY} = $e->{$COLO_KEY} 
        		if exists $e->{$COLO_KEY};
        	$opts->{$CORRECT_KEY} = $e->{$CORRECT_KEY} 
        		if exists $e->{$CORRECT_KEY};
        	$opts->{$INDEP_KEY} = $e->{$INDEP_KEY} 
        		if exists $e->{$INDEP_KEY};
        	warn "No relationship type for relationship edge!" unless $opts->{'type'};
        	my( $ok, @result ) = $collation->add_relationship( $from->{$IDKEY}, $to->{$IDKEY}, $opts );
        	unless( $ok ) {
        		warn "Did not add relationship: @result";
        	}
        } 
    }

    ## Deal with node information (transposition, relationships, etc.) that
    ## needs to be processed after all the nodes are created.
    ## TODO unneeded after conversion
    unless( $use_version ) {
		print STDERR "Adding second-pass node data\n";
		foreach my $nkey ( keys %$extra_data ) {
			foreach my $edkey ( keys %{$extra_data->{$nkey}} ) {
				my $this_reading = $collation->reading( $nkey );
				if( $edkey eq $TRANSPOS_KEY ) {
					my $other_reading = $collation->reading( $extra_data->{$nkey}->{$edkey} );
					$this_reading->set_identical( $other_reading );
				} else {
					warn "Unfamiliar reading node data $edkey for $nkey";
				}
			}
		}
    }
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
