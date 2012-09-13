#!/usr/bin/perl

use strict;
use warnings;
use feature 'unicode_strings';
use lib '/home/tla/stemmatology/lib';
use Encode qw/ decode_utf8 /;
use Fcntl qw/ :flock /;
use Gearman::Worker;
use Graph;
use Graph::Reader::Dot;
use Text::Tradition::Directory;
use Text::Tradition::Stemma;
use IPC::Run qw/ run /;
use JSON;
use TryCatch;

my $db = Text::Tradition::Directory->new(
    'dsn' => 'dbi:mysql:dbname=idpresult',
    'extra_args' => { 'user' => 'stemmaweb', 'password' => 'l@chmann' } );
my @idp_programs = qw/ findGroupings findClasses /;
# there is also findSources but it is redundant for now
my $witness_map = {};

my $worker = Gearman::Worker->new();
$worker->job_servers('127.0.0.1');
$worker->register_function( run_idp => \&run_idp );
$worker->work while 1;

# Handle a request to run IDP on a list of Text::Tradition::Analysis::Result
# object IDs. Need to look these up in the DB, set their status to 'running',
# convert them to JSON, and send them off to be solved.

sub run_idp {
    my $job = shift;
    print "Beginning IDP run for ID(s) " . $job->arg . "\n";
    my @problemids = split( /\s*,\s*/, $job->arg );
    my $scope = $db->new_scope();
    # Look up each problem ID and sort them into distinct groups by graph.
    my %distinct_graphs;
    my %dgproblems;  # lookup table for problem ID -> set mapping
    my $denom = 0;   # the total number of problems we will solve
    # Clear out the witness map
    $witness_map = {};
    foreach my $problem ( @problemids ) {
        my $result = $db->lookup( $problem );
        if( $result ) {
            # Check to see if it already has an answer
            if( $result->status && $result->status eq 'OK' ) {
                print STDERR "Solution already recorded for Analysis::Result problem $problem\n";
                next;
            } elsif( $result->status && $result->status eq 'running' ) {
                print STDERR "Already working on Analysis::Result problem $problem\n";
                next;
            }
            # No? Then add it to our list.
            $denom++;
            $result->status( 'running' );
            $db->save( $result );
            $distinct_graphs{$result->graph} = [] 
                unless exists $distinct_graphs{$result->graph};
            push( @{$distinct_graphs{$result->graph}}, [ $result->sets ] );
            $dgproblems{$result->graph} = [] 
                unless exists $dgproblems{$result->graph};
            push( @{$dgproblems{$result->graph}}, $problem );
        } else {
            print STDERR "Did not find Analysis::Result with ID $problem; skipping\n";
        }
    }

    my $done = 0;
    # Now for each graph problem set, sanitize it, convert it to JSON,
    # and send it to IDP.
    foreach my $dg ( keys %distinct_graphs ) {
        my $idpdata = { graph => $dg, groupings => $distinct_graphs{$dg} };
        my $datastr = encode_json( _sanitize_names( $idpdata ) );
        my %idpanswer;
        foreach my $program ( @idp_programs ) {
            # Got the data, so send it to IDP.
            $ENV{'PATH'} = '/bin:/usr/bin:/usr/local/bin';
            chdir('/usr/lib/byzantinist/idp');
            my @cmd = qw! idp -e !;
            push( @cmd, "exec($program)", 'main.idp' );
            my( $ret, $err );
            run( \@cmd, \$datastr, \$ret, \$err );
            
            if( $err =~ /^Error:/m ) {
                print STDERR "Error running idp: $err\n";
                return;
            }
        
            # Save the result for the given program
            $idpanswer{$program} = _desanitize_names( decode_json( $ret ) );
        }
        # Now map the results from IDP back into the database.
        foreach my $idx ( 0 .. $#{$dgproblems{$dg}} ) {
            my $result = $db->lookup( $dgproblems{$dg}->[$idx] );
            my $genanswer = $idpanswer{'findGroupings'}->[$idx];
            $result->is_genealogical( $genanswer->[1] ? 1 : 0 );

            # We take the groupings as well as the classes from the 
            # findClasses answer, to make sure they match
            my $classanswer = $idpanswer{'findClasses'}->[$idx];
            foreach my $grouping ( @{$classanswer->[0]} ) {
                $result->record_grouping( $grouping );
            }
            foreach my $class ( keys %{$classanswer->[1]} ) {
                my $class_members = $classanswer->[1]->{$class};
                map { $result->set_class( $_, $class ) } @$class_members;
            }
            $result->status('OK');
            print "Saving new IDP result with ID key " . $result->object_key . "\n";
            $db->save( $result );
        }
        
        # Update the job status if we have more than one problem
        if( scalar keys %distinct_graphs > 1 ) {
            $done += scalar @{$dgproblems{$dg}};
            $job->set_status( $done, $denom );
        }
    }
    return $done;
}

sub _sanitize_names {
    my( $element ) = @_;
    my $result;
    if( ref( $element ) eq 'HASH' ) {
        my $safe_hash = {};
        map { my $k = $_; $safe_hash->{$k} = _sanitize_names( $element->{$k} ) } keys %$element;
        $result = $safe_hash;
    } elsif( ref( $element ) eq 'ARRAY' || ref( $element ) eq 'Set::Scalar' ) {
        $result = [];
        foreach my $n ( @$element ) {
            push( @$result, _sanitize_names( $n ) );
        }
    } elsif( $element =~ /^digraph/ ) {
        my $dotfh;
        open( $dotfh, '<', \$element );
	binmode( $dotfh, ':utf8' );
        my $graph = Graph::Reader::Dot->new()->read_graph( $dotfh );
        die "Could not parse graph from dot: $element" unless $graph;
        # Make a new graph with safe witness names
        my $cgraph = Graph->new();
        foreach my $v ( $graph->vertices ) {
            my $nv = _sanitize_names( $v );
            $cgraph->add_vertex( $nv );
            $cgraph->set_vertex_attribute( $nv, 'class',
                $graph->get_vertex_attribute( $v, 'class' ) );
        }
        foreach my $e ( $graph->edges ) {
            my $ne = _sanitize_names( $e );
            $cgraph->add_edge( @$ne );
        }
        # Now as well as sanitizing the graph we should prune away
        # hypothetical leaf nodes, to optimize the problem for the solver.
        _prune_hypotheticals( $cgraph );
        # Write the cloned graph out to a string
        $result = Text::Tradition::Stemma::editable_graph( $cgraph, { linesep => ' ' } );
        $witness_map->{$result} = $element unless $result eq $element;
    } else {
        $result = _safe_witstr( $element );
        if( $result ne $element ) {
            # Warn if witness_map conflicts
            warn "Ambiguous transformation $result for $element vs. " 
                . $witness_map->{$result}
                if( exists( $witness_map->{$result} ) 
                    && $witness_map->{$result} ne $element );
            $witness_map->{$result} = $element;
        }
    }
    return $result;
}

sub _safe_witstr {
    my $witstr = shift;
    $witstr =~ s/\s+/_/g;
    $witstr =~ s/[^\w\d-]//g;
    return $witstr;
}

sub _desanitize_names {
    my( $element ) = @_;
    my $result = [];
    if( ref( $element ) eq 'ARRAY' ) {
        foreach my $n ( @$element ) {
            push( @$result, _desanitize_names( $n ) );
        }
    } elsif( ref( $element ) eq 'HASH' ) {
        my $real_hash = {};
        map { $real_hash->{$_} = _desanitize_names( $element->{$_} ) } keys %$element;
        $result = $real_hash;
    } elsif( exists $witness_map->{$element} ) {
        $result = $witness_map->{$element}
    } else {
        $result = $element;
    }
    return $result;
}

sub _prune_hypotheticals {
    my $graph = shift;
    my @orphan_hypotheticals;
    do {
        @orphan_hypotheticals = 
            grep { $graph->get_vertex_attribute( $_, 'class' ) 
                       eq 'hypothetical' } $graph->successorless_vertices;
        $graph->delete_vertices( @orphan_hypotheticals );
    } while( @orphan_hypotheticals );
}
