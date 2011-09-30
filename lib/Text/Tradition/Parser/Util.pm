package Text::Tradition::Parser::Util;

use strict;
use warnings;
use Algorithm::Diff;
use Exporter 'import';
use vars qw/ @EXPORT_OK /;
@EXPORT_OK = qw/ add_hash_entry check_for_repeated cmp_str collate_variants is_monotonic /;

=item B<collate_variants>

collate_variants( $collation, @reading_ranges )

Given a set of readings in the form 
( lemma_start, lemma_end, rdg1_start, rdg1_end, ... )
walks through each to identify those readings that are identical.  The
collation is a Text::Tradition::Collation object; the elements of
@readings are Text::Tradition::Collation::Reading objects that appear
on the collation graph.

TODO: Handle collapsed and non-collapsed transpositions.

=cut

sub collate_variants {
    my( $collation, @reading_sets ) = @_;

    # Two different ways to do this, depending on whether we want
    # transposed reading nodes to be merged into one (producing a
    # nonlinear, bidirectional graph) or not (producing a relatively
    # linear, unidirectional graph.)
    return $collation->linear ? collate_linearly( @_ )
        : collate_nonlinearly( @_ );
}

sub collate_linearly {
    my( $collation, $lemma_set, @variant_sets ) = @_;

    my @unique;
    my $substitutions = {};
    push( @unique, @$lemma_set );
    while( @variant_sets ) {
        my $variant_set = shift @variant_sets;
        # Use diff to do this job
        my $diff = Algorithm::Diff->new( \@unique, $variant_set, 
                                         {'keyGen' => \&_collation_hash} );
        my @new_unique;
        my %merged;
        while( $diff->Next ) {
            if( $diff->Same ) {
                # merge the nodes
                my @l = $diff->Items( 1 );
                my @v = $diff->Items( 2 );
                foreach my $i ( 0 .. $#l ) {
                    if( !$merged{$l[$i]->name} ) {
                        print STDERR sprintf( "Merging %s into %s\n", 
                                             $v[$i]->name,
                                             $l[$i]->name );
                        $collation->merge_readings( $l[$i], $v[$i] );
                        $merged{$l[$i]->name} = 1;
                        $substitutions->{$v[$i]->name} = $l[$i];
                    } else {
                        print STDERR "Would have double merged " . $l[$i]->name . "\n";
                    }
                }
                # splice the lemma nodes into the variant set
                my( $offset ) = $diff->Get( 'min2' );
                splice( @$variant_set, $offset, scalar( @l ), @l );
                push( @new_unique, @l );
            } else {
                # Keep the old unique readings
                push( @new_unique, $diff->Items( 1 ) ) if $diff->Items( 1 );
                # Add the new readings to the 'unique' list
                push( @new_unique, $diff->Items( 2 ) ) if $diff->Items( 2 );
            }
        }
        @unique = @new_unique;
    }
    return $substitutions;
}

sub collate_nonlinearly {
    my( $collation, $lemma_set, @variant_sets ) = @_;
    
    my @unique;
    my $substitutions = {};
    push( @unique, @$lemma_set );
    while( @variant_sets ) {
        my $variant_set = shift @variant_sets;
        # Simply match the first reading that carries the same word, so
        # long as that reading has not yet been used to match another
        # word in this variant. That way lies loopy madness.
        my @distinct;
        my %merged;
        foreach my $idx ( 0 .. $#{$variant_set} ) {
            my $vw = $variant_set->[$idx];
            my @same = grep { cmp_str( $_ ) eq $vw->label } @unique;
            my $matched;
            if( @same ) {
                foreach my $i ( 0 .. $#same ) {
                    unless( $merged{$same[$i]->name} ) {
                        #print STDERR sprintf( "Merging %s into %s\n", 
                        #                     $vw->name,
                        #                     $same[$i]->name );
                        $collation->merge_readings( $same[$i], $vw );
                        $merged{$same[$i]->name} = 1;
                        $matched = $i;
                        $variant_set->[$idx] = $same[$i];
                        $substitutions->{$vw->name} = $same[$i];
                    }
                }
            }
            unless( @same && defined($matched) ) {
                push( @distinct, $vw );
            }
        }
        push( @unique, @distinct );
    }
    return $substitutions;
}

sub _collation_hash {
    my $node = shift;
    return cmp_str( $node );
}

=item B<cmp_str>

Pretend you never saw this method.  Really it needs to not be hardcoded.

=cut

sub cmp_str {
    my( $reading ) = @_;
    my $word = $reading->label();
    $word = lc( $word );
    $word =~ s/\W//g;
    $word =~ s/v/u/g;
    $word =~ s/j/i/g;
    $word =~ s/cha/ca/g;
    $word =~ s/quatuor/quattuor/g;
    $word =~ s/ioannes/iohannes/g;
    return $word;
}

=item B<collate_variants>

my @rep = check_for_repeated( @readings )

Given an array of items, returns any items that appear in the array more
than once.

=cut

sub check_for_repeated {
    my @seq = @_;
    my %unique;
    my @repeated;
    foreach ( @seq ) {
        if( exists $unique{$_->name} ) {
            push( @repeated, $_->name );
        } else {
            $unique{$_->name} = 1;
        }
    }
    return @repeated;
}

sub add_hash_entry {
    my( $hash, $key, $entry ) = @_;
    if( exists $hash->{$key} ) {
        push( @{$hash->{$key}}, $entry );
    } else {
        $hash->{$key} = [ $entry ];
    }
}

sub is_monotonic {
    my( @readings ) = @_;
    my( $common, $min, $max ) = ( -1, -1, -1 );
    foreach my $rdg ( @readings ) {
#         print STDERR "Checking reading " . $rdg->name . "/" . $rdg->text . " - " 
#         . $rdg->position->reference ."\n";
        return 0 if $rdg->position->common < $common;
        if( $rdg->position->common == $common ) {
            return 0 if $rdg->position->min <= $min;
            return 0 if $rdg->position->max <= $max;
        }
        $common = $rdg->position->common;
        $min = $rdg->position->min;
        $max = $rdg->position->max;
    }
    return 1;
}