package Text::Tradition::StemmaUtil;

use strict;
use warnings;
use Exporter 'import';
use vars qw/ @EXPORT_OK /;
@EXPORT_OK = qw/ phylip_pars_input /;

sub make_character_matrix {
    my( $table ) = @_;
    # Push the names of the witnesses to initialize the rows of the matrix.
    my @matrix = map { [ _normalize_witname( $_->{'witness'} ) ] } 
    				@{$table->{'alignment'}};
    foreach my $token_index ( 0 .. $table->{'length'} - 1) {
        # First implementation: make dumb alignment table, caring about
        # nothing except which reading is in which position.
        my @pos_readings = map { $_->{'tokens'}->[$token_index] }
        						@{$table->{'alignment'}};
        my @pos_text = map { $_ ? $_->{'t'} : $_ } @pos_readings;
        my @chars = convert_characters( \@pos_text );
        foreach my $idx ( 0 .. $#matrix ) {
            push( @{$matrix[$idx]}, $chars[$idx] );
        }
    }
    return \@matrix;
} 

# Helper function to make the witness name something legal for pars

sub _normalize_witname {
    my( $witname ) = @_;
    $witname =~ s/\s+/ /g;
    $witname =~ s/[\[\]\(\)\:;,]//g;
    $witname = substr( $witname, 0, 10 );
    return sprintf( "%-10s", $witname );
}

sub convert_characters {
    my $row = shift;
    # This is a simple algorithm that treats every reading as different.
    # Eventually we will want to be able to specify how relationships
    # affect the character matrix.
    my %unique = ( '__UNDEF__' => 'X',
                   '#LACUNA#'  => '?',
                 );
    my %count;
    my $ctr = 0;
    foreach my $word ( @$row ) {
        if( $word && !exists $unique{$word} ) {
            $unique{$word} = chr( 65 + $ctr );
            $ctr++;
        }
        $count{$word}++ if $word;
    }
    # Try to keep variants under 8 by lacunizing any singletons.
    if( scalar( keys %unique ) > 8 ) {
		foreach my $word ( keys %count ) {
			if( $count{$word} == 1 ) {
				$unique{$word} = '?';
			}
		}
    }
    my %u = reverse %unique;
    if( scalar( keys %u ) > 8 ) {
        warn "Have more than 8 variants on this location; phylip will break";
    }
    my @chars = map { $_ ? $unique{$_} : $unique{'__UNDEF__' } } @$row;
    return @chars;
}

sub phylip_pars_input {
    my $table = shift;
    my $character_matrix = make_character_matrix( $table );
    my $input = '';
    my $rows = scalar @{$character_matrix};
    my $columns = scalar @{$character_matrix->[0]} - 1;
    $input .= "\t$rows\t$columns\n";
    foreach my $row ( @{$character_matrix} ) {
        $input .= join( '', @$row ) . "\n";
    }
    return $input;
}

