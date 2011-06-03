package Text::Tradition::Parser::CSV;

use strict;
use warnings;
use Storable qw /dclone/;
use Text::CSV::Simple;

=head1 NAME

Text::Tradition::Parser::CSV

=head1 DESCRIPTION

Parser module for Text::Tradition, given a list of variants as a CSV
file and a reference text as a plaintext file with appropriate line
breaks.

=head1 METHODS

=over

=item B<read>

my @apparatus = read( $csv_file );

Takes a CSV file; returns a data structure of apparatus entries to be
merged with a base text.

=cut

sub read {
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
	    # Was it a doubled-up apparatus entry e.g 'non (1/2)'?
	    if( keys %$apparatus &&
		$apparatus->{'rdg_0'} =~ /(.*?)\s+\(?([\d\/]+)\)?\s*$/ ) {
		my( $reading, $istr ) = ( $1, $2 );
		my @instances = split( /\//, $istr );
		foreach my $i ( @instances ) {
		    my $app = dclone( $apparatus );
		    $app->{'rdg_0'} = $reading . "_$i";
		    $app->{'_id'} .= chr(97+$i);
		    push( @app_list, $app );
		}
	    } elsif( keys %$apparatus ) {
		push( @app_list, $apparatus );
	    }
	    $apparatus = { _id => $linehash{reference} };
	    $rdg_ctr = 0;
	}
	# The apparatus has multiple readings, and multiple witnesses per
	# reading.  So it's a hashref whose values are listrefs.
	$apparatus->{'rdg_0'} = $linehash{'text'} if $linehash{'text'};
	$apparatus->{'rdg_' . ++$rdg_ctr} = $linehash{'variant'};
	foreach my $attr ( @fields[3..8] ) {
	    $apparatus->{"_rdg_${rdg_ctr}_$attr"} = $linehash{$attr} if defined $linehash{$attr};
	}
	
	foreach my $k ( @fields[10..$#fields] ) {
	    my $variant_rdg = $linehash{$k};
	    $k =~ s/\s+\(a\.c\.\)//;
	    if( $variant_rdg =~ /^0/ ) {
		$apparatus->{$k} = 'rdg_0'
		    unless exists $apparatus->{$k};
	    } elsif ( $variant_rdg =~ /^1/ ) {
		warn sprintf( "Already found variant reading %s for %s at %s!",
			      $apparatus->{$k}, $k, $apparatus->{_id} )
		    if exists $apparatus->{$k} && $apparatus->{$k} ne 'rdg_0';
		$apparatus->{$k} = 'rdg_' . $rdg_ctr;
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

=back

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews, aurum@cpan.org

=cut

1;

