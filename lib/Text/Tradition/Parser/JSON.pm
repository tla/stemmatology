package Text::Tradition::Parser::JSON;

use strict;
use warnings;
use JSON qw/ from_json /;

=head1 NAME

Text::Tradition::Parser::JSON

=head1 SYNOPSIS

  use Text::Tradition;
  
  my $tradition = Text::Tradition->new( 
    'name' => 'my text',
    'input' => 'JSON',
    'string' => $json_encoded_utf8,
    );

=head1 DESCRIPTION

Parser module for Text::Tradition to read a JSON alignment table format such
as that produced by CollateX.

=head1 METHODS

=head2 B<parse>( $tradition, $option_hash )

Takes an initialized tradition and a set of options; creates the
appropriate nodes and edges on the graph, as well as the appropriate
witness objects.  The $option_hash must contain either a 'file' or a
'string' argument with the JSON structure to be parsed.

The structure of the JSON is thus:

 { alignment => [ { witness => "SIGIL", 
                    tokens => [ { t => "TEXT" }, ... ] },
                  { witness => "SIG2", 
                    tokens => [ { t => "TEXT" }, ... ] },
                    ... ],
 };


Longer lacunae in the text, to be disregarded in cladistic analysis, may be 
represented with the meta-reading '#LACUNA#'.  Multiple lacuna tags in sequence
are collapsed into a single multi-reading lacuna.

If a witness name ends in the collation's ac_label, it will be treated as
an extra layer of the 'main' witness whose sigil it shares.

=begin testing

use Text::Tradition;
binmode STDOUT, ":utf8";
binmode STDERR, ":utf8";
eval { no warnings; binmode $DB::OUT, ":utf8"; };

use_ok( 'Text::Tradition::Parser::JSON' );

open( JSFILE, 't/data/cx16.json' );
binmode JSFILE, ':utf8';
my @lines = <JSFILE>;
close JSFILE;

my $t = Text::Tradition->new(
    'name' => 'json',
    'input' => 'JSON',
    'string' => join( '', @lines ),
);

is( ref( $t ), 'Text::Tradition', "Parsed a JSON alignment" );
if( $t ) {
    is( scalar $t->collation->readings, 26, "Collation has all readings" );
    is( scalar $t->collation->paths, 32, "Collation has all paths" );
    is( scalar $t->witnesses, 3, "Collation has all witnesses" );
}

=end testing

=cut

sub parse {
	my( $tradition, $opts ) = @_;
	my $c = $tradition->collation;
	
	my $table = from_json( $opts->{'string'} );
	
	# Create the witnesses
    my @witnesses;
    my %ac_wits;  # Track these for later removal
    foreach my $sigil ( map { $_->{'witness'} } @{$table->{'alignment'}} ) {
        my $wit = $tradition->add_witness( 'sigil' => $sigil );
        $wit->path( [ $c->start ] );
        push( @witnesses, $wit );
        my $aclabel = $c->ac_label;
        if( $sigil =~ /^(.*)\Q$aclabel\E$/ ) {
            $ac_wits{$1} = $wit;
        }
    }

	# Create the readings in each row
    my $length = exists $table->{'length'}
    	? $table->{'length'}
    	: scalar @{$table->{'alignment'}->[0]->{'tokens'}};
    
    foreach my $idx ( 0 .. $length - 1 ) {
    	my @tokens = map { $_->{'tokens'}->[$idx] } @{$table->{'alignment'}};
        my @readings = make_nodes( $c, $idx, @tokens );
        foreach my $w ( 0 .. $#readings ) {
            # push the appropriate node onto the appropriate witness path
            my $rdg = $readings[$w];
            if( $rdg ) {
                my $wit = $witnesses[$w];
                push( @{$wit->path}, $rdg );
            } # else skip it for empty readings.
        }
    }
    
    # Collapse our lacunae into a single node and
    # push the end node onto all paths.
    $c->end->rank( $length );
    foreach my $wit ( @witnesses ) {
        my $p = $wit->path;
        my $last_rdg = shift @$p;
        my $new_p = [ $last_rdg ];
        foreach my $rdg ( @$p ) {
        	# Omit the reading if we are in a lacuna already.
        	next if $rdg->is_lacuna && $last_rdg->is_lacuna;
			# Save the reading otherwise.
			push( @$new_p, $rdg );
			$last_rdg = $rdg;
        }
        push( @$new_p, $c->end );
        $wit->path( $new_p );
    }
    
    # Fold any a.c. witnesses into their main witness objects, and
    # delete the independent a.c. versions.
    foreach my $a ( keys %ac_wits ) {
        my $main_wit = $tradition->witness( $a );
        next unless $main_wit;
        my $ac_wit = $ac_wits{$a};
        $main_wit->uncorrected_path( $ac_wit->path );
        $tradition->del_witness( $ac_wit );
    }
    
    # Join up the paths.
    $c->make_witness_paths;
    # Delete our unused lacuna nodes.
	foreach my $rdg ( grep { $_->is_lacuna } $c->readings ) {
		$c->del_reading( $rdg ) unless $c->reading_witnesses( $rdg );
	}
}

=head2 make_nodes( $collation, $index, @tokenlist )

Create readings from the unique tokens in @tokenlist, and set their rank to
$index.  Returns an array of readings of the same size as the original @tokenlist.

=cut

sub make_nodes {
	my( $c, $idx, @tokens ) = @_;
	my %unique;
	my $ctr = 1;
	foreach my $t ( @tokens ) {
		next unless $t;
		my $id = join( ',', $idx, $ctr++ );
		my $rdg = Text::Tradition::Collation::Reading->new( 
			'id' => $id, 'json' => $t, 'collation' => $c );
		my $comptoken = $c->collapse_punctuation ? $rdg->text 
			: $rdg->punctuated_form;
		$unique{$comptoken} = $rdg;
		$t->{'comptoken'} = $comptoken;
	}
	map { $c->add_reading( $_ ) } values( %unique );
	return map { $_ && $unique{$_->{'comptoken'}} } @tokens;
}

1;

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews E<lt>aurum@cpan.orgE<gt>
