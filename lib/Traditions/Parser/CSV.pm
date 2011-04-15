package Traditions::Parser::CSV;

use strict;
use warnings;
use Text::CSV::Simple;
use Traditions::Parser::BaseText qw( merge_base );

# Takes a CSV file and a base text; returns a GraphML object.

sub parse {
    my( $graph, $csv_file, $base_text ) = @_;

    # Parse the CSV file into a list of apparatus entries.
    my @app_list = read_csv( $csv_file );
    # Now put the base text onto the graph, and merge in the 
    # apparatus entries.
    merge_base( $graph, $base_text, @app_list );
}

# Takes a CSV file; returns a data structure of apparatus entries to
# be merged with a base text.

sub read_csv {
    my( $csv_file ) = @_;
    my $parser = Text::CSV::Simple->new();
    my @fields = qw/ reference text variant type context non_corr non_indep 
                     length total origin /;
    my @lines = $parser->read_file( $ARGV[0] );
    my @labels = @{shift( @lines )};
    push( @fields, @labels[10..$#labels] );

    my $started = 0;
    my $rdg_ctr = 0;
    my $apparatus = {};
    my @app_list;
    foreach my $line ( @lines ) {
	my $new_lemma = 0;
	if( $line->[0] =~ /^\d/ ) {
	    $new_lemma = $started = 1;
	}
	next unless $started;
	
	# Get the lines into their fields.
	my %linehash;
	@linehash{@fields} = @$line;
	
	# Readings can take up multiple lines in the CSV, so append the
	# apparatus to the list, and clear it out, if we have started a
	# new reading.
	if( $new_lemma ) {
	    push( @app_list, $apparatus ) if keys %$apparatus;
	    $apparatus = { _id => $linehash{reference},
	    };
	    $rdg_ctr = 0;
	}
	# The apparatus has multiple readings, and multiple witnesses per
	# reading.  So it's a hashref whose values are listrefs.
	$apparatus->{ 'rdg_0' } = $linehash{ 'text' } 
        if $linehash{ 'text' };
	$apparatus->{ 'rdg_' . ++$rdg_ctr } = $linehash{ 'variant' };
	foreach my $attr ( @fields[3..8] ) {
	    $apparatus->{"_rdg_${rdg_ctr}_$attr"} = $linehash{ $attr }
	    if $linehash{ $attr };
	}
	
	foreach my $k ( @fields[10..$#fields] ) {
	    my $variant_rdg = $linehash{$k};
	    $k =~ s/\s+\(a\.c\.\)//;
	    if( $variant_rdg =~ /^0/ ) {
		$apparatus->{ $k } = 'rdg_0'
		    unless exists $apparatus->{ $k };
	    } elsif ( $variant_rdg =~ /^1/ ) {
		$apparatus->{ $k } = 'rdg_' . $rdg_ctr;
	    } else { # else for $, we don't list the MS
		warn "Unparsed variant indicator $variant_rdg for $k in " .
		    $apparatus->{'_id'}
	        unless ( !$variant_rdg or $variant_rdg =~ /^\$$/ );
	    }
	}
	# See if we have at least one reading for each variant.
	my @seen_rdgs = values %$apparatus;
	foreach my $rdg ( grep { $_ =~ /^rdg/ } keys %$apparatus ) {
	    unless( grep { $_ =~ /^$rdg$/ } @seen_rdgs ) {
		print STDERR 'No manuscript found with reading "' 
		    . $apparatus->{$rdg} .
		    '" at location ' . $apparatus->{_id} . "\n";
		# delete $apparatus->{$rdg}; # for now
	    }
	}
    }
    # Done with loop, so push the last apparatus.
    push( @app_list, $apparatus );
    return @app_list;
}

1;

