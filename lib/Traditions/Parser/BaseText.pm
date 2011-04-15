package Traditions::Parser::BaseText;

use strict;
use warnings;
use Exporter 'import';
use vars qw( @EXPORT_OK );
@EXPORT_OK = qw( merge_base );

sub merge_base {
    my( $graph, $base_file, @app_entries ) = @_;
    my @base_line_starts = read_base( $base_file, $graph );

    foreach my $app ( @app_entries ) {
	my( $line, $num ) = split( /\./, $app->{_id} );
	# DEBUG with a short graph
	# last if $line > 2;
	my $scrutinize = "21.8";
	my $first_line_node = $base_line_starts[ $line ];
	my $too_far = $base_line_starts[ $line+1 ];
	
	my $lemma = $app->{rdg_0};
	my $seq = 1; 
	# Is this the Nth occurrence of this reading in the line?
	if( $lemma =~ s/(_)?(\d)$// ) {
	    $seq = $2;
	}
	my @lemma_words = split( /\s+/, $lemma );
	
	# Now search for the lemma words within this line.
	my $lemma_start = $first_line_node;
	my $lemma_end;
	my %seen;
	while( $lemma_start ne $too_far ) {
	    # Loop detection
	    if( $seen{ $lemma_start->name() } ) {
		warn "Detected loop at " . $lemma_start->name() . 
		    ", ref $line,$num";
		last;
	    }
	    $seen{ $lemma_start->name() } = 1;
	    
	    # Try to match the lemma.
	    my $unmatch = 0;
	    print STDERR "Matching " . cmp_str( $lemma_start) . " against " .
		$lemma_words[0] . "...\n"
		if "$line.$num" eq $scrutinize;
	    if( cmp_str( $lemma_start ) eq $lemma_words[0] ) {
		# Skip it if we need a match that is not the first.
		if( --$seq < 1 ) {
		    # Now we have to compare the rest of the words here.
		    if( scalar( @lemma_words ) > 1 ) {
			my $next_node = $graph->next_word( $lemma_start );
			foreach my $w ( @lemma_words[1..$#lemma_words] ) {
			    printf STDERR "Now matching %s against %s\n", 
				    cmp_str($next_node), $w
				if "$line.$num" eq $scrutinize;
			    if( $w ne cmp_str($next_node) ) {
				$unmatch = 1;
				last;
			    } else {
				$lemma_end = $next_node;
				$next_node = $graph->next_word( $lemma_end );
			    }
			}
		    } else {
			$lemma_end = $lemma_start;
		    }
		} else {
		    $unmatch = 1;
		}
	    }
	    last unless ( $unmatch || !defined( $lemma_end ) );
	    $lemma_end = undef;
	    $lemma_start = $graph->next_word( $lemma_start );
	}
	
	unless( $lemma_end ) {
	    warn "No match found for @lemma_words at $line.$num";
	    next;
	} else {
	    # These are no longer common nodes; unmark them as such.
	    my @lemma_nodes = $graph->node_sequence( $lemma_start, 
						     $lemma_end );
	    map { $_->set_attribute( 'class', 'lemma' ) } @lemma_nodes;
	}
	
	# Now we have our lemma nodes; we add the variant nodes to the graph.
	
	# For each reading that is not rdg_0, we make a chain of nodes
	# and connect them to the anchor.  Edges are named after the mss
	# that are relevant.
	foreach my $k ( grep { /^rdg/ } keys( %$app ) ) {
	    next if $k eq 'rdg_0'; # that's the lemma.
	    my @variant = split( /\s+/, $app->{$k} );
	    @variant = () if $app->{$k} eq '/'; # This is an omission.
	    my @mss = grep { $app->{$_} eq $k } keys( %$app );
	    
	    unless( @mss ) {
		print STDERR "Skipping '@variant' at $line.$num: no mss\n";
		next;
	    }
	    
	    # Determine the label name for the edges here.
	    my $edge_name = join(', ', @mss );
	    
	    # Make the variant into a set of nodes.
	    my $ctr = 0;
	    my $last_node = $graph->prior_word( $lemma_start );
	    my $var_start;
	    foreach my $vw ( @variant ) {
		my $vwname = "$k/$line.$num.$ctr"; $ctr++;
		my $vwnode = $graph->add_node( $vwname );
		$vwnode->set_attribute( 'label', $vw );
		$vwnode->set_attribute( 'class', 'variant' );
		$graph->add_edge( $last_node, $vwnode, $edge_name );
		$var_start = $vwnode unless $var_start;
		$last_node = $vwnode;
	    }
	    # Now hook it up at the end.
	    $graph->add_edge( $last_node, $graph->next_word( $lemma_end ),
					$edge_name );
	    
	    # Now collate and collapse the identical nodes within the graph.
	    collate_variant( $graph, $lemma_start, $lemma_end, 
			     $var_start, $last_node );
	    
	}
    }

    ## Now in theory I have a graph.  I want to make it a little easier to
    ## read.  So I collapse nodes that have only one edge in and one edge
    ## out, and I do this by looking at the edges.
    
    foreach my $edge ( $graph->edges() ) {
	my @out_edges = $edge->from()->outgoing();
	my @in_edges = $edge->to()->incoming();
	
	next unless scalar( @out_edges ) == 1;
	next unless scalar( @in_edges ) == 1;
	next unless $out_edges[0] eq $in_edges[0];
	# In theory if we've got this far, we're safe, but just to
	# double-check...
	next unless $out_edges[0] eq $edge;
	
	$graph->merge_nodes( $edge->from(), $edge->to(), ' ' );
    }
}

# read_base: Takes a text file and a (presumed empty) graph object,
# adds the words as simple linear nodes to the graph, and returns a
# list of nodes that represent the beginning of lines. This graph is
# now the starting point for application of apparatus entries in
# merge_base, e.g. from a CSV file or a CTE file.

sub read_base {
    my( $base_file, $graph ) = @_;
    
    # This array gives the first node for each line.  We put the
    # common starting point in line zero.
    my $last_node = $graph->start();
    my $lineref_array = [ $last_node ]; # There is no line zero.

    open( BASE, $base_file ) or die "Could not open file $base_file: $!";
    while(<BASE>) {
	# Make the nodes, and connect them up for the base, but also
	# save the first node of each line in an array for the purpose.
	chomp;
	my @words = split;
	my $started = 0;
	my $wordref = 0;
	my $lineref = scalar @$lineref_array;
	foreach my $w ( @words ) {
	    my $noderef = join( ',', $lineref, ++$wordref );
	    my $node = $graph->add_node( $noderef );
	    $node->set_attribute( 'label', $w );
	    $node->set_attribute( 'class', 'common' );
	    unless( $started ) {
		push( @$lineref_array, $node );
		$started = 1;
	    }
	    if( $last_node ) {
		$graph->add_edge( $last_node, $node, "base text" );
		$last_node = $node;
	    } # TODO there should be no else here...
	}
    }
    close BASE;
    # Ending point for all texts
    my $endpoint = $graph->add_node( '#END#' );
    $graph->add_edge( $last_node, $endpoint, "base text" );
    push( @$lineref_array, $endpoint );

    return( @$lineref_array );
}


## Helper methods for merge_base

sub collate_variant {
    my( $graph, $lemma_start, $lemma_end, $var_start, $var_end ) = @_;
    # If var_start is undef, then the variant is an omission and
    # there's nothing to collate. Return.
    return unless $var_start;

    # I want to look at the nodes in the variant and lemma, and
    # collapse nodes that are the same word.  This is mini-collation.
    my %collapsed = ();
    # There will only be one outgoing edge at first, so this is safe.
    my @out = $var_start->outgoing();
    my $var_label = $out[0]->label();

    my @lemma_nodes;
    while( $lemma_start ne $lemma_end ) {
	push( @lemma_nodes, $lemma_start );
	$lemma_start = $graph->next_word( $lemma_start );
    } 
    push( @lemma_nodes, $lemma_end );
    
    my @variant_nodes;
    while( $var_start ne $var_end ) {
	push( @variant_nodes, $var_start );
	$var_start = $graph->next_word( $var_start, $var_label );
    }
    push( @variant_nodes, $var_end );

    # Go through the variant nodes, and if we find a lemma node that
    # hasn't yet been collapsed with a node, equate them.

    foreach my $w ( @variant_nodes ) {
	my $word = $w->label();
	foreach my $l ( @lemma_nodes ) {
	    if( $word eq cmp_str( $l ) ) {
		next if exists( $collapsed{ $l->label } )
		    && $collapsed{ $l->label } eq $l;
		# Collapse the nodes.
		printf STDERR "Merging nodes %s/%s and %s/%s\n", 
		    $l->name, $l->label, $w->name, $w->label;
		$graph->merge_nodes( $l, $w );
		$collapsed{ $l->label } = $l;
		# Now collapse any multiple edges to and from the node.
		# Rely on the presence of the 'base text' edge.
		remove_duplicate_edges( $graph, $graph->prior_word( $l ), $l );
		remove_duplicate_edges( $graph, $l, $graph->next_word( $l ) );
	    }
	}
    }
}

sub remove_duplicate_edges {
    my( $graph, $from, $to ) = @_;
    my @edges = $from->edges_to( $to );
    if( scalar @edges > 1 ) {
	my @base = grep { $_->label eq 'base text' } @edges;
	if ( scalar @base ) {
	    # Remove the edges that are not base.
	    foreach my $e ( @edges ) {
		$graph->del_edge( $e )
		    unless $e eq $base[0];
	    }
	} else {
	    # Combine the edges into one.
	    my $new_edge_name = join( ', ', map { $_->label() } @edges );
	    my $new_edge = shift @edges;
	    $new_edge->set_attribute( 'label', $new_edge_name );
	    foreach my $e ( @edges ) {
		$graph->del_edge( $e );
	    }
	}
    }
}

# TODO need to make this configurable!
sub cmp_str {
    my( $node ) = @_;
    my $word = $node->label();
    $word = lc( $word );
    $word =~ s/\W//g;
    $word =~ s/v/u/g;
    $word =~ s/j/i/g;
    $word =~ s/cha/ca/g;
    $word =~ s/quatuor/quattuor/g;
    $word =~ s/ioannes/iohannes/g;
    return $word;
}

1;
