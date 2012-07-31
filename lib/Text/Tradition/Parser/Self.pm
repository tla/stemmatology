package Text::Tradition::Parser::Self;

use strict;
use warnings;
use Text::Tradition::Parser::GraphML qw/ graphml_parse /;
use TryCatch;

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

is( ref( $t ), 'Text::Tradition', "Parsed GraphML version 2" );
if( $t ) {
    is( scalar $t->collation->readings, 319, "Collation has all readings" );
    is( scalar $t->collation->paths, 376, "Collation has all paths" );
    is( scalar $t->witnesses, 13, "Collation has all witnesses" );
}

# TODO add a relationship, write graphml, reparse it, check that the rel
# is still there
$t->language('Greek');
$t->collation->add_relationship( 'w12', 'w13', 
	{ 'type' => 'grammatical', 'scope' => 'global', 
	  'annotation' => 'This is some note' } );
ok( $t->collation->get_relationship( 'w12', 'w13' ), "Relationship set" );
my $graphml_str = $t->collation->as_graphml;

my $newt = Text::Tradition->new( 'input' => 'Self', 'string' => $graphml_str );
is( ref( $newt ), 'Text::Tradition', "Parsed current GraphML version" );
if( $newt ) {
    is( scalar $newt->collation->readings, 319, "Collation has all readings" );
    is( scalar $newt->collation->paths, 376, "Collation has all paths" );
    is( scalar $newt->witnesses, 13, "Collation has all witnesses" );
    is( scalar $newt->collation->relationships, 1, "Collation has added relationship" );
    is( $newt->language, 'Greek', "Tradition has correct language setting" );
    my $rel = $newt->collation->get_relationship( 'w12', 'w13' );
    ok( $rel, "Found set relationship" );
    is( $rel->annotation, 'This is some note', "Relationship has its properties" );
}


=end testing

=cut
use Data::Dump;
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
    my $tmeta = $tradition->meta;
    my $cmeta = $collation->meta;
    foreach my $gkey ( keys %{$graph_data->{'global'}} ) {
		my $val = $graph_data->{'global'}->{$gkey};
		if( $gkey eq 'version' ) {
			$use_version = $val;
		} elsif( $tmeta->has_attribute( $gkey ) ) {
			$tradition->$gkey( $val );
		} else {
			$collation->$gkey( $val );
		}
	}
		
    # Add the nodes to the graph. 

    # print STDERR "Adding collation readings\n";
    foreach my $n ( @{$graph_data->{'nodes'}} ) {    	
    	# If it is the start or end node, we already have one, so
    	# grab the rank and go.
        if( defined $n->{'is_start'} ) {
	  warn Data::Dump::dump($n);
	  warn $collation->start->id;
	  $collation->start->rank($n->{'rank'});
          next;
        }
    	if( defined $n->{'is_end'} ) {
	  warn Data::Dump::dump($n);
    		$collation->end->rank( $n->{'rank'} );
    		next;
    	}
		my $gnode = $collation->add_reading( $n );
    }
        
    # Now add the edges.
    # print STDERR "Adding collation path edges\n";
    foreach my $e ( @{$graph_data->{'edges'}} ) {
      # warn Data::Dump::dump([$collation->reading_keys]);
       warn $e->{source}{id};
      # warn $e->{target}{id};
       my($from, $to);
       if($e->{source}{is_start}) {
         $from = $collation->start;
       } elsif($e->{source}{is_end}) {
	 $from = $collation->end;
       } else {
         $from = $collation->reading( $e->{'source'}->{'id'} );
       }
       if($e->{target}{is_end}) {
	 $to = $collation->end;
       } elsif($e->{target}{is_start}) {
	 $to = $collation->start;
       } else {
         $to = $collation->reading( $e->{'target'}->{'id'} );
       }

		warn "No witness label on path edge!" unless $e->{'witness'};
		my $label = $e->{'witness'} . ( $e->{'extra'} ? $collation->ac_label : '' );
		$collation->add_path( $from, $to, $label );
		
		# Add the witness if we don't have it already.
		unless( $witnesses{$e->{'witness'}} ) {
			$tradition->add_witness( 
				sigil => $e->{'witness'}, 'sourcetype' => 'collation' );
			$witnesses{$e->{'witness'}} = 1;
		}
		$tradition->witness( $e->{'witness'} )->is_layered( 1 ) if $e->{'extra'};
    }
    
    ## Done with the main graph, now look at the relationships.
	# Nodes are added via the call to add_reading above.  We only need
	# add the relationships themselves.
	# TODO check that scoping does trt
	$rel_data->{'edges'} ||= []; # so that the next line doesn't break on no rels
	foreach my $e ( sort { _layersort_rel( $a, $b ) } @{$rel_data->{'edges'}} ) {
		my $from = $collation->reading( $e->{'source'}->{'id'} );
		my $to = $collation->reading( $e->{'target'}->{'id'} );
		delete $e->{'source'};
		delete $e->{'target'};
		# The remaining keys are relationship attributes.
		# Backward compatibility...
		if( $use_version eq '2.0' || $use_version eq '3.0' ) {
			delete $e->{'class'};
			$e->{'type'} = delete $e->{'relationship'} if exists $e->{'relationship'};
		}
		# Add the specified relationship unless we already have done.
		my $rel_exists;
		if( $e->{'scope'} ne 'local' ) {
			my $relobj = $collation->get_relationship( $from, $to );
			if( $relobj && $relobj->scope eq $e->{'scope'}
				&& $relobj->type eq $e->{'type'} ) {
				$rel_exists = 1;
			}
		}
		try {
			$collation->add_relationship( $from, $to, $e ) unless $rel_exists;
		} catch( Text::Tradition::Error $e ) {
			warn "DROPPING $from -> $to: " . $e->message;
		}
	}
	
    # Save the text for each witness so that we can ensure consistency
    # later on
	$collation->text_from_paths();	
}

## Return the relationship that comes first in priority.
my %LAYERS = (
	'collated' => 1,
	'orthographic' => 2,
	'spelling' => 3,
	);

sub _layersort_rel {
	my( $a, $b ) = @_;
	my $key = exists $a->{'type'} ? 'type' : 'relationship';
	my $at = $LAYERS{$a->{$key}} || 99;
	my $bt = $LAYERS{$b->{$key}} || 99;
	return $at <=> $bt;
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
