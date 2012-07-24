#!/usr/bin/env perl

use feature 'say';
use lib 'lib';
use strict;
use warnings;
use Text::Tradition::Collation::Reading::Lexeme;
use Text::Tradition::Directory;
use TryCatch;
use utf8;

binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';
eval { no warnings; binmode $DB::OUT, ':utf8'; $DB::deep = 1000 };

my $dir = Text::Tradition::Directory->new(
    'dsn' => 'dbi:SQLite:dbname=stemmaweb/db/traditions.db',
    );

my $scope = $dir->new_scope();
my @traditions;
my $lookfor = $ARGV[0] || '';
foreach my $tinfo ( $dir->traditionlist ) {
    next unless $tinfo->{id} eq $lookfor || $tinfo->{name} =~ /$lookfor/;
    my $tradition = $dir->lookup( $tinfo->{id} );
    print STDERR "Found " . $tradition->name . "\n";
    my $c = $tradition->collation;
    $c->_set_tradition( $tradition );
    # Make the change(s)
    say STDERR "First run:";
    do_the_work( $c );
    say STDERR "Second run:";
    do_the_work( $c );

    # Save the lot
    $dir->save( $tradition );
}

sub do_the_work {
    my $c = shift;
    foreach my $rel ( $c->relationships ) {
        my $relobj = $c->get_relationship( $rel );
        next unless $relobj->scope eq 'global';

        # For any global relationship, the relationship store should contain an 
        # entry in $self->scopedrels for the text of the two related readings.
        # The scoped relationship is case insensitive unless the relationship type
        # is 'orthographic'. The object of this script is to detect when a global
        # relationship object is not the one in $self->scopedrels, and replace it
        # with the one that is.

        # Case insensitive unless orthographic...
        my $rdga = $relobj->type eq 'orthographic' 
            ? $relobj->reading_a : lc( $relobj->reading_a );
        my $rdgb = $relobj->type eq 'orthographic' 
            ? $relobj->reading_b : lc( $relobj->reading_b );

        # Get the applicable scoped relationship object
        my $scopeobj = $c->relations->scoped_relationship( $rdga, $rdgb );

        # Test to see whether it exists, corresponds with $relobj, and if it is not
        # identical to $relobj then replace $relobj with it.
        if( $scopeobj ) {
            if( $scopeobj->type ne $relobj->type ) {
                say STDERR sprintf( "Different scoped relationship types at %s - %s: %s vs. %s",
                             @$rel, $scopeobj->type, $relobj->type );
            } elsif( ( $scopeobj->reading_a ne $relobj->reading_a )
                     || ( $scopeobj->reading_b ne $relobj->reading_b) ) {
                say STDERR "Different a/b readings for scoped object at @$rel";
            } elsif( $scopeobj ne $relobj ) {
                say STDERR "Replacing " . $relobj->type . " relobj with scoped object at @$rel";
                $c->relations->_set_relationship( $scopeobj, @$rel );
            }
        } else {
            say STDERR "No scoped object for relationship at @$rel";
        }

    }
}
