#!/usr/bin/env perl

use warnings;
use strict;

=head1 DESCRIPTION

Makes sure that all of the modules that are 'use'd are listed in the
Makefile.PL as dependencies.  Also as long as we are source filtering,
make sure there are no $DB::single statements in the code.

=cut

use Test::More;
use File::Find;
eval 'use Module::CoreList';
if ($@) { plan skip_all => 'Module::CoreList not installed' }

plan 'no_plan';

my %skipped;
if( -f 'MANIFEST.SKIP' ) {
	# We don't want these
	open( SKIP, 'MANIFEST.SKIP' ) or die "Could not open manifest skip file";
	while(<SKIP>) {
		chomp;
		$skipped{$_} = 1;
	}
	close SKIP;
}
my %used;
find( \&wanted, qw/ lib t / );

sub wanted {
    return unless -f $_;
    return if $File::Find::dir  =~ m!/.git($|/)!;
    return if $File::Find::name =~ /~$/;
    return if $File::Find::name =~ /\.(pod|html)$/;
    return if $skipped{$File::Find::name};

    # read in the file from disk
    my $filename = $_;
    local $/;
    open( FILE, $filename ) or return;
    my $data = <FILE>;
    close(FILE);

    # strip pod, in a really idiotic way.  Good enough though
    $data =~ s/^=(begin|head).+?(^=cut|\Z)//gms;

    # look for use and use base statements
    $used{$1}{$File::Find::name}++ while $data =~ /^\s*use\s+([\w:]+)/gm;
    while ( $data =~ m|^\s*use base qw.([\w\s:]+)|gm ) {
        $used{$_}{$File::Find::name}++ for split ' ', $1;
    }
    # look for DB statements while we are here
    while( $data =~ /^\s*\$DB::single/gm ) {
    	fail( "DB::single statement present in source " . $File::Find::name );
    }
}

my %required;
{
    local $/;
    ok( open( MAKEFILE, "Makefile.PL" ), "Opened Makefile" );
    my $data = <MAKEFILE>;
    close(FILE);
    while ( $data =~ /^\s*?(?:requires|recommends|).*?([\w:]+)'(?:\s*=>\s*['"]?([\d\.]+)['"]?)?.*?(?:#(.*))?$/gm ) {
        $required{$1} = $2;
        if ( defined $3 and length $3 ) {
            $required{$_} = undef for split ' ', $3;
        }
    }
}

for ( sort keys %used ) {
    my $first_in = Module::CoreList->first_release($_);
    next if defined $first_in and $first_in <= 5.00803;
    next if /^(Text::Tradition|inc|t|feature|parent)(::|$)/;

    #warn $_;
    ok( exists $required{$_}, "$_ in Makefile.PL" )
        or diag( "used in ", join ", ", sort keys %{ $used{$_} } );
    delete $used{$_};
    delete $required{$_};
}

for ( sort keys %required ) {
    my $first_in = Module::CoreList->first_release( $_, $required{$_} );
    fail("Required module $_ (v. $required{$_}) is in core since $first_in")
        if defined $first_in and $first_in <= 5.008003;
}

1;

