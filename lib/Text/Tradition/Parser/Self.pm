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
    is( scalar $t->collation->paths, 2854, "Collation has all paths" );
    is( scalar $t->witnesses, 13, "Collation has all witnesses" );
}

=end testing

=cut

my( $IDKEY, $TOKENKEY, $TRANSPOS_KEY, $RANK_KEY, $CLASS_KEY,
	$SOURCE_KEY, $TARGET_KEY, $WITNESS_KEY, $EXTRA_KEY, $RELATIONSHIP_KEY ) 
    = qw/ name reading identical rank class 
    	  source target witness extra relationship/;

sub parse {
    my( $tradition, $opts ) = @_;
    my $graph_data = graphml_parse( $opts );
    
    my $collation = $tradition->collation;
    my %witnesses;
    
    # Set up the graph-global attributes.  They will appear in the
    # hash under their accessor names.
    print STDERR "Setting graph globals\n";
    $tradition->name( $graph_data->{'name'} );
    foreach my $gkey ( keys %{$graph_data->{'attr'}} ) {
		my $val = $graph_data->{'attr'}->{$gkey};
		$collation->$gkey( $val );
	}
		
    # Add the nodes to the graph. 

    my $extra_data = {}; # Keep track of data that needs to be processed
                         # after the nodes & edges are created.
    print STDERR "Adding graph nodes\n";
    foreach my $n ( @{$graph_data->{'nodes'}} ) {
    	# First extract the data that we can use without reference to
    	# anything else.
    	my %node_data = %$n; # Need $n itself untouched for edge processing
        my $nodeid = delete $node_data{$IDKEY};
        my $reading = delete $node_data{$TOKENKEY};
        my $class = delete $node_data{$CLASS_KEY} || '';
        my $rank = delete $node_data{$RANK_KEY};
        
        # Create the node.  Current valid classes are common and meta. 
        # Everything else is a normal reading.
        my $gnode = $collation->add_reading( $nodeid );
        $gnode->text( $reading );
        $gnode->make_common if $class eq 'common';
        $gnode->is_meta( 1 ) if $class eq 'meta';
        # This is a horrible hack.
        $gnode->is_lacuna( $reading =~ /^\#LACUNA/ );
        $gnode->rank( $rank ) if defined $rank;

        # Now save the data that we need for post-processing,
        # if it exists.
        if ( keys %node_data ) {
            $extra_data->{$nodeid} = \%node_data
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
        	$opts->{'equal_rank'} = $e->{'equal_rank'} 
        		if exists $e->{'equal_rank'};
        	$opts->{'non_correctable'} = $e->{'non_correctable'} 
        		if exists $e->{'non_correctable'};
        	$opts->{'non_independent'} = $e->{'non_independent'} 
        		if exists $e->{'non_independent'};
        	warn "No relationship type for relationship edge!" unless $opts->{'type'};
        	my( $ok, @result ) = $collation->add_relationship( $from->{$IDKEY}, $to->{$IDKEY}, $opts );
        	unless( $ok ) {
        		warn "Did not add relationship: @result";
        	}
        } 
    }

    ## Deal with node information (transposition, relationships, etc.) that
    ## needs to be processed after all the nodes are created.
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
