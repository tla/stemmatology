package Text::Tradition::Collation::Position;

use Moose;

has 'common' => (
    is => 'rw',
    isa => 'Int',
    required => 1,
    );

has 'min' => (
    is => 'rw',
    isa => 'Int',
    required => 1,
    );

has 'max' => (
    is => 'rw',
    isa => 'Int',
    required => 1,
    );

# This gets set if we are tracking a more specifically-positioned
# reading.
has 'matched' => (
    is => 'rw',
    isa => 'Bool',
    );

around BUILDARGS => sub {
    my $orig = shift;
    my $class = shift;

    # Two ways we can be called - with the arguments we expect, or with a
    # single argument to be parsed out into a position.
    my %args;
    if( @_ == 1 ) {
	my( $common, $min, $max ) = parse_reference( $_[0] );
	%args = ( 'common' => $common,
		  'min' => $min,
		  'max' => $max );
    } elsif ( 2 <= @_ && @_ <= 3 ) {
	my( $common, $min, $max ) = @_;
	$max = $min unless $max;
	%args = ( 'common' => $common,
		  'min' => $min,
		  'max' => $max );
    } else {
	%args = @_;
    }

    return $class->$orig( %args );
};

sub BUILD {
    my $self = shift;
    if( $self->min > $self->max ) {
	die "Position minimum cannot be higher than maximum";
    }
}

sub parse_reference {
    my( $ref ) = @_;
    if( $ref =~ /^(\d+),(\d+)(\-(\d+))?$/ ) {
	my( $common, $min, $max ) = ( $1, $2, $4 );
	$max = $min unless defined $max;
	return( $common, $min, $max );
    } else {
	warn "Bad argument $ref passed to Position constructor";
	return undef;
    }
}

# Instance method
sub cmp_with {
    my( $self, $other ) = @_;
    return _cmp_bits( [ $self->common, $self->min, $self->max ],
		      [ $other->common, $other->min, $other->max ] );
}

# Class method
sub str_cmp {
    my( $a, $b ) = @_;
    my @abits = parse_reference( $a );
    my @bbits = parse_reference( $b );
    return _cmp_bits( \@abits, \@bbits );
}

sub _cmp_bits {
    my( $a, $b ) = @_;
    return $a->[0] <=> $b->[0]
	unless $a->[0] == $b->[0];
    return $a->[1] <=> $b->[1]
	unless $a->[1] == $b->[1];
    return $a->[2] <=> $b->[2];
}

sub minref {
    my $self = shift;
    return join(',', $self->common, $self->min );
}

sub maxref {
    my $self = shift;
    return join(',', $self->common, $self->max );
}

sub reference {
    my $self = shift;
    my $answer = join( ',', $self->common, $self->min );
    $answer .= '-'. $self->max unless $self->min == $self->max;
    return $answer;
}

sub fixed {
    my $self = shift;
    return $self->min == $self->max;
}

sub is_colocated {
    my( $self, $other, $strict ) = @_;
    if( $strict ) {
	return $self->common == $other->common
	    && $self->min == $other->min
	    && $self->max == $other->max;
    } else {
	return $self->common == $other->common 
	    && $self->min <= $other->max
	    && $self->max >= $other->min;
    }
}

# Return all the possible fixed position refs.
sub possible_positions {
    my $self = shift;
    my @possible = map { join( ',', $self->common, $_ ) } ( $self->min .. $self->max );
    return @possible;
}

no Moose;
__PACKAGE__->meta->make_immutable;
