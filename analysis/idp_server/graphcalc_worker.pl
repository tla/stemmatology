#!/usr/bin/perl

use strict;
use warnings;
use feature qw/ say unicode_strings /;
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

my %VARS = (
	DBTYPE => 'mysql',
	DBHOST => '127.0.0.1',
	DBPORT => '3006',
	DBNAME => 'idpresult',
	DSN => undef,
	DBUSER => undef,
	DBPASS => undef,
	GEARMAN_SERVER => '127.0.0.1:4730',
	IDPBINPATH => '/usr/local/idp/bin',
	IDPSCRIPTPATH => '/usr/local/idp/script'
);

if( -f "/etc/graphcalc.conf" ) {
	# Read the variables in from here.
	open( GCCONF, "/etc/graphcalc.conf" ) 
		or die "Could not open configuration file /etc/graphcalc.conf";
	while(<GCCONF>) {
		chomp;
		s/^\s+//;
		my( $name, $val ) = split( /\s*\=\s*/, $_ );
		if( exists $VARS{$name} ) {
			$VARS{$name} = $val;
		}
	}
	close GCCONF;
}
unless( $VARS{DSN} ) {
	$VARS{DSN} = sprintf( "dbi:%s:dbname=%s;host=%s;port=%s",
		$VARS{DBTYPE}, $VARS{DBNAME}, $VARS{DBHOST}, $VARS{DBPORT} );
}

my $db = Text::Tradition::Directory->new(
    'dsn' => $VARS{DSN}, 
    'extra_args' => { 'user' => $VARS{DBUSER}, 'password' => $VARS{DBPASS} } );
my @idp_programs = qw/ findGroupings findClasses /;
# there is also findSources but it is redundant for now
my $witness_map = {};

my $worker = Gearman::Worker->new();
$worker->job_servers( $VARS{GEARMAN_SERVER} );
$worker->register_function( run_idp => \&run_idp );
$worker->work while 1;

# Handle a request to run IDP on a list of Text::Tradition::Analysis::Result
# object IDs. Need to look these up in the DB, set their status to 'running',
# convert them to JSON, and send them off to be solved.

sub run_idp {
    my $job = shift;
    say scalar( localtime( time() ) ) . "\tBeginning IDP run for ID(s) " . $job->arg;
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
                say STDERR "Solution already recorded for Analysis::Result problem $problem";
                next;
            } elsif( $result->status && $result->status eq 'running' ) {
                say STDERR "Already working on Analysis::Result problem $problem";
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
            say STDERR "Did not find Analysis::Result with ID $problem; skipping";
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
            $ENV{'PATH'} = '/bin:/usr/bin:/usr/local/bin:'.$VARS{IDPBINPATH};
            chdir( $VARS{IDPSCRIPTPATH} );
            my @cmd = qw! idp -e !;
            push( @cmd, "exec($program)", 'main.idp' );
            my( $ret, $err );
            run( \@cmd, \$datastr, \$ret, \$err );
            
            my $got_error;
            say STDERR "IDP run output:\n$err";
            if( $err =~ / Error:/m ) {
                $idpanswer{$program} = 'error';
                say STDERR "Input data was " . decode_utf8( $datastr );
            } else {
				# Save the result for the given program
				try {
					$idpanswer{$program} = _desanitize_names( decode_json( $ret ) );
				} catch {
					say STDERR "Could not parse string '$ret' as JSON";
					$idpanswer{$program} = 'error';
				}
			}
        }
        # Now map the results from IDP back into the database.
        foreach my $idx ( 0 .. $#{$dgproblems{$dg}} ) {
            my $result = $db->lookup( $dgproblems{$dg}->[$idx] );
            my $groupstatus = $idpanswer{'findGroupings'};
            my $classstatus = $idpanswer{'findClasses'};
            if( $groupstatus eq 'error' || $classstatus eq 'error' ) {
            	$result->status('error');
            } else {
				my $genanswer = $groupstatus->[$idx];
				$result->is_genealogical( $genanswer->[1] ? 1 : 0 );

				# We take the groupings as well as the classes from the 
				# findClasses answer, to make sure they match
				my $classanswer = $classstatus->[$idx];
				foreach my $grouping ( @{$classanswer->[0]} ) {
					$result->record_grouping( $grouping );
				}
				foreach my $class ( keys %{$classanswer->[1]} ) {
					my $class_members = $classanswer->[1]->{$class};
					map { $result->set_class( $_, $class ) } @$class_members;
				}
				$result->status('OK');
				say "Saving new IDP result with ID key " . $result->object_key;
			}
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
