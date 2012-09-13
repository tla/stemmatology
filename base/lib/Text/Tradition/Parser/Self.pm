package Text::Tradition::Parser::Self;

use strict;
use warnings;
use Text::Tradition::Parser::GraphML qw/ graphml_parse /;
use Text::Tradition::UserStore;
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

use File::Temp;
use Safe::Isa;
use Test::Warn;
use Text::Tradition;
use TryCatch;
binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";
eval { no warnings; binmode $DB::OUT, ":utf8"; };

my $tradition = 't/data/florilegium_graphml.xml';
my $t = Text::Tradition->new( 
    'name'  => 'inline', 
    'input' => 'Self',
    'file'  => $tradition,
    );

ok( $t->$_isa('Text::Tradition'), "Parsed GraphML version 2" );
if( $t ) {
    is( scalar $t->collation->readings, 319, "Collation has all readings" );
    is( scalar $t->collation->paths, 376, "Collation has all paths" );
    is( scalar $t->witnesses, 13, "Collation has all witnesses" );
}

# TODO add a relationship, add a stemma, write graphml, reparse it, check that 
# the new data is there
$t->language('Greek');
my $stemma_enabled;
try {
	$stemma_enabled = $t->enable_stemmata;
} catch {
	ok( 1, "Skipping stemma tests without Analysis module" );
}
if( $stemma_enabled ) {
	$t->add_stemma( 'dotfile' => 't/data/florilegium.dot' );
}
$t->collation->add_relationship( 'w12', 'w13', 
	{ 'type' => 'grammatical', 'scope' => 'global', 
	  'annotation' => 'This is some note' } );
ok( $t->collation->get_relationship( 'w12', 'w13' ), "Relationship set" );
my $graphml_str = $t->collation->as_graphml;

my $newt = Text::Tradition->new( 'input' => 'Self', 'string' => $graphml_str );
ok( $newt->$_isa('Text::Tradition'), "Parsed current GraphML version" );
if( $newt ) {
    is( scalar $newt->collation->readings, 319, "Collation has all readings" );
    is( scalar $newt->collation->paths, 376, "Collation has all paths" );
    is( scalar $newt->witnesses, 13, "Collation has all witnesses" );
    is( scalar $newt->collation->relationships, 1, "Collation has added relationship" );
    is( $newt->language, 'Greek', "Tradition has correct language setting" );
    my $rel = $newt->collation->get_relationship( 'w12', 'w13' );
    ok( $rel, "Found set relationship" );
    is( $rel->annotation, 'This is some note', "Relationship has its properties" );
    if( $stemma_enabled ) {
	    is( scalar $newt->stemmata, 1, "Tradition has its stemma" );
    	is( $newt->stemma(0)->witnesses, $t->stemma(0)->witnesses, "Stemma has correct length witness list" );
    }
}

# Test user save / restore
my $fh = File::Temp->new();
my $file = $fh->filename;
$fh->close;
my $dsn = "dbi:SQLite:dbname=$file";
my $userstore = Text::Tradition::Directory->new( { dsn => $dsn,
	extra_args => { create => 1 } } );
my $scope = $userstore->new_scope();
my $testuser = $userstore->create_user( { url => 'http://example.com' } );
ok( $testuser->$_isa('Text::Tradition::User'), "Created test user via userstore" );
$testuser->add_tradition( $newt );
is( $newt->user->id, $testuser->id, "Assigned tradition to test user" );
$graphml_str = $newt->collation->as_graphml;
my $usert;
warning_is {
	$usert = Text::Tradition->new( 'input' => 'Self', 'string' => $graphml_str );
} 'DROPPING user assignment without a specified userstore',
	"Got expected user drop warning on parse";
$usert = Text::Tradition->new( 'input' => 'Self', 'string' => $graphml_str,
	'userstore' => $userstore );
is( $usert->user->id, $testuser->id, "Parsed tradition with userstore points to correct user" );

# Test warning if we can
unless( $stemma_enabled ) {
	my $nst;
	warnings_exist {
		$nst = Text::Tradition->new( 'input' => 'Self', 'file' => 't/data/lexformat.xml' );
	} [qr/DROPPING stemmata/],
		"Got expected stemma drop warning on parse";
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
		} elsif( $gkey eq 'stemmata' ) {
			# Make sure we can handle stemmata
			my $stemma_enabled;
			try {
				$stemma_enabled = $tradition->enable_stemmata;
			} catch {
				warn "Analysis module not installed; DROPPING stemmata";
			}
			# Parse the stemmata into objects
			if( $stemma_enabled ) {
				foreach my $dotstr ( split( /\n/, $val ) ) {
					$tradition->add_stemma( 'dot' => $dotstr );
				}
			}
		} elsif( $gkey eq 'user' ) {
			# Assign the tradition to the user if we can
			if( exists $opts->{'userstore'} ) {
				my $userdir = delete $opts->{'userstore'};
				my $user = $userdir->find_user( { username => $val } );
				if( $user ) {
					$user->add_tradition( $tradition );
				} else {
					warn( "Found no user with ID $val; DROPPING user assignment" );
				}
			} else {
				warn( "DROPPING user assignment without a specified userstore" );
			}
		} elsif( $tmeta->has_attribute( $gkey ) ) {
			$tradition->$gkey( $val );
		} else {
			$collation->$gkey( $val );
		}
	}
		
    # Add the nodes to the graph.
    # Note any reading IDs that were changed in order to comply with XML 
    # name restrictions; we have to hardcode start & end.
    my %namechange = ( '#START#' => '__START__', '#END#' => '__END__' );

    # print STDERR "Adding collation readings\n";
    foreach my $n ( @{$graph_data->{'nodes'}} ) {    	
    	# If it is the start or end node, we already have one, so
    	# grab the rank and go.
        if( defined $n->{'is_start'} ) {
#	  warn Data::Dump::dump($n);
#	  warn $collation->start->id;
	  $collation->start->rank($n->{'rank'});
          next;
        }
    	if( defined $n->{'is_end'} ) {
#	  warn Data::Dump::dump($n);
    		$collation->end->rank( $n->{'rank'} );
    		next;
    	}
		my $gnode = $collation->add_reading( $n );
		if( $gnode->id ne $n->{'id'} ) {
			$namechange{$n->{'id'}} = $gnode->id;
		}
    }
        
    # Now add the edges.
    # print STDERR "Adding collation path edges\n";
    foreach my $e ( @{$graph_data->{'edges'}} ) {
    	my $sourceid = exists $namechange{$e->{'source'}->{'id'}}
    		? $namechange{$e->{'source'}->{'id'}} : $e->{'source'}->{'id'};
    	my $targetid = exists $namechange{$e->{'target'}->{'id'}}
    		? $namechange{$e->{'target'}->{'id'}} : $e->{'target'}->{'id'};
        my $from = $collation->reading( $sourceid );
        my $to = $collation->reading( $targetid );

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
    	my $sourceid = exists $namechange{$e->{'source'}->{'id'}}
    		? $namechange{$e->{'source'}->{'id'}} : $e->{'source'}->{'id'};
    	my $targetid = exists $namechange{$e->{'target'}->{'id'}}
    		? $namechange{$e->{'target'}->{'id'}} : $e->{'target'}->{'id'};
        my $from = $collation->reading( $sourceid );
        my $to = $collation->reading( $targetid );
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
