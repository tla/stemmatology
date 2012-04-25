package Text::Tradition::Collation::Reading;

use Moose;
use overload '""' => \&_stringify, 'fallback' => 1;

=head1 NAME

Text::Tradition::Collation::Reading - represents a reading (usually a word)
in a collation.

=head1 DESCRIPTION

Text::Tradition is a library for representation and analysis of collated
texts, particularly medieval ones.  A 'reading' refers to a unit of text,
usually a word, that appears in one or more witnesses (manuscripts) of the
tradition; the text of a given witness is composed of a set of readings in
a particular sequence

=head1 METHODS

=head2 new

Creates a new reading in the given collation with the given attributes.
Options include:

=over 4

=item collation - The Text::Tradition::Collation object to which this
reading belongs.  Required.

=item id - A unique identifier for this reading. Required.

=item text - The word or other text of the reading.

=item is_start - The reading is the starting point for the collation.

=item is_end - The reading is the ending point for the collation.

=item is_lacuna - The 'reading' represents a known gap in the text.

=item is_ph - A temporary placeholder for apparatus parsing purposes.  Do
not use unless you know what you are doing.

=item rank - The sequence number of the reading. This should probably not
be set manually.

=back

One of 'text', 'is_start', 'is_end', or 'is_lacuna' is required.

=head2 collation

=head2 id

=head2 text

=head2 is_start

=head2 is_end

=head2 is_lacuna

=head2 rank

Accessor methods for the given attributes.

=cut

has 'collation' => (
	is => 'ro',
	isa => 'Text::Tradition::Collation',
	# required => 1,
	weak_ref => 1,
	);

has 'id' => (
	is => 'ro',
	isa => 'Str',
	required => 1,
	);

has 'text' => (
	is => 'ro',
	isa => 'Str',
	required => 1,
	writer => 'alter_text',
	);
	
has 'language' => (
	is => 'ro',
	isa => 'Str',
	default => 'Default',
	);
	
has 'is_start' => (
	is => 'ro',
	isa => 'Bool',
	default => undef,
	);

has 'is_end' => (
	is => 'ro',
	isa => 'Bool',
	default => undef,
	);
    
has 'is_lacuna' => (
    is => 'ro',
    isa => 'Bool',
	default => undef,
    );
    
has 'is_ph' => (
	is => 'ro',
	isa => 'Bool',
	default => undef,
	);
	
has 'is_common' => (
	is => 'rw',
	isa => 'Bool',
	default => undef,
	);

has 'rank' => (
    is => 'rw',
    isa => 'Int',
    predicate => 'has_rank',
    clearer => 'clear_rank',
    );
    
## For morphological analysis

has 'normal_form' => (
	is => 'rw',
	isa => 'Str',
	predicate => 'has_normal_form',
	);

# Holds the word form. If is_disambiguated is true, the form at index zero
# is the correct one.
has 'reading_lexemes' => (
	traits => ['Array'],
	isa => 'ArrayRef[Text::Tradition::Collation::Reading::Lexeme]',
	handles => {
		lexemes => 'elements',
		has_lexemes => 'count',
		clear_lexemes => 'clear',
		add_lexeme => 'push',
		},
	default => sub { [] },
	);
	
## For prefix/suffix readings

has 'join_prior' => (
	is => 'ro',
	isa => 'Bool',
	default => undef,
	);
	
has 'join_next' => (
	is => 'ro',
	isa => 'Bool',
	default => undef,
	);


around BUILDARGS => sub {
	my $orig = shift;
	my $class = shift;
	my $args;
	if( @_ == 1 ) {
		$args = shift;
	} else {
		$args = { @_ };
	}
			
	# If one of our special booleans is set, we change the text and the
	# ID to match.
	if( exists $args->{'is_lacuna'} && !exists $args->{'text'} ) {
		$args->{'text'} = '#LACUNA#';
	} elsif( exists $args->{'is_start'} ) {
		$args->{'id'} = '#START#';  # Change the ID to ensure we have only one
		$args->{'text'} = '#START#';
		$args->{'rank'} = 0;
	} elsif( exists $args->{'is_end'} ) {
		$args->{'id'} = '#END#';	# Change the ID to ensure we have only one
		$args->{'text'} = '#END#';
	} elsif( exists $args->{'is_ph'} ) {
		$args->{'text'} = $args->{'id'};
	}
	
	$class->$orig( $args );
};

=head2 is_meta

A meta attribute (ha ha), which should be true if any of our 'special'
booleans are true.  Implies that the reading does not represent a bit 
of text found in a witness.

=cut

sub is_meta {
	my $self = shift;
	return $self->is_start || $self->is_end || $self->is_lacuna || $self->is_ph;	
}

=head1 Convenience methods

=head2 related_readings

Calls Collation's related_readings with $self as the first argument.

=cut

sub related_readings {
	my $self = shift;
	return $self->collation->related_readings( $self, @_ );
}

=head2 witnesses 

Calls Collation's reading_witnesses with $self as the first argument.

=cut

sub witnesses {
	my $self = shift;
	return $self->collation->reading_witnesses( $self, @_ );
}

=head2 predecessors

Returns a list of Reading objects that immediately precede $self in the collation.

=cut

sub predecessors {
	my $self = shift;
	my @pred = $self->collation->sequence->predecessors( $self->id );
	return map { $self->collation->reading( $_ ) } @pred;
}

=head2 successors

Returns a list of Reading objects that immediately follow $self in the collation.

=cut

sub successors {
	my $self = shift;
	my @succ = $self->collation->sequence->successors( $self->id );
	return map { $self->collation->reading( $_ ) } @succ;
}

=head2 set_identical( $other_reading)

Backwards compatibility method, to add a transposition relationship
between $self and $other_reading.  Don't use this.

=cut

sub set_identical {
	my( $self, $other ) = @_;
	return $self->collation->add_relationship( $self, $other, 
		{ 'type' => 'transposition' } );
}

sub _stringify {
	my $self = shift;
	return $self->id;
}

=head1 MORPHOLOGY

A few methods to try to tack on morphological information.

=head2 use_lexemes

TBD

=cut

# sub use_lexemes {
# 	my( $self, @lexemes ) = @_;
# 	# The lexemes need to be the same as $self->text.
# 	my $cmpstr = $self->has_normal_form ? lc( $self->normal_form ) : lc( $self->text );
# 	$cmpstr =~ s/[\s-]+//g;
# 	my $lexstr = lc( join( '', @lexemes ) );
# 	$lexstr =~ s/[\s-]+//g;
# 	unless( $lexstr eq $cmpstr ) {
# 		warn "Cannot split " . $self->text . " into " . join( '.', @lexemes );
# 		return;
# 	}
# 	$self->_clear_morph;
# 	map { $self->_add_morph( { $_ => [] } ) } @lexemes;
# }
# 
# sub add_morphological_tag {
# 	my( $self, $lexeme, $opts ) = @_;
# 	my $struct;
# 	unless( $opts ) {
# 		# No lexeme was passed; use reading text.
# 		$opts = $lexeme;
# 		$lexeme = $self->text;
# 		$self->use_lexemes( $lexeme );
# 	}
# 	# Get the correct container
# 	( $struct ) = grep { exists $_->{$lexeme} } $self->lexemes;
# 	unless( $struct ) {
# 		warn "No lexeme $lexeme exists in this reading";
# 		return;
# 	}
# 	# Now make the morph object and add it to this lexeme.
# 	my $morph_obj = Text::Tradition::Collation::Reading::Morphology->new( $opts );
# 	# TODO Check for existence
# 	push( @{$struct->{$lexeme}}, $morph_obj );
# }

## Utility methods

sub TO_JSON {
	my $self = shift;
	return $self->text;
}

## TODO will need a throw() here

no Moose;
__PACKAGE__->meta->make_immutable;

###################################################
### Morphology objects, to be attached to readings
###################################################

package Text::Tradition::Collation::Reading::Morphology;

use Moose;

has 'lemma' => (
	is => 'ro',
	isa => 'Str',
	required => 1,
	);
	
has 'code' => (
	is => 'ro',
	isa => 'Str',
	required => 1,
	);
	
has 'language' => (
	is => 'ro',
	isa => 'Str',
	required => 1,
	);
	
## Transmute codes into comparison arrays for our various languages.

around BUILDARGS => sub {
	my $orig = shift;
	my $class = shift;
	my $args;
	if( @_ == 1 && ref( $_[0] ) ) {
		$args = shift;
	} else {
		$args = { @_ };
	}
	if( exists( $args->{'serial'} ) ) {
		my( $lemma, $code ) = split( /!!/, delete $args->{'serial'} );
		$args->{'lemma'} = $lemma;
		$args->{'code'} = $code;
	}
	$class->$orig( $args );
};

sub serialization {
	my $self = shift;
	return join( '!!', $self->lemma, $self->code );
};

sub comparison_array {
	my $self = shift;
	if( $self->language eq 'French' ) {
		my @array;
		my @bits = split( /\+/, $self->code );
		# First push the non k/v parts.
		while( @bits && $bits[0] !~ /=/ ) {
			push( @array, shift @bits );
		}
		while( @array < 2 ) {
			push( @array, undef );
		}
		# Now push the k/v parts in a known order.
		my @fields = qw/ Pers Nb Temps Genre Spec Fonc /;
		my %props;
		map { my( $k, $v ) = split( /=/, $_ ); $props{$k} = $v; } @bits;
		foreach my $k ( @fields ) {
			push( @array, $props{$k} );
		}
		# Give the answer.
		return @array;
	} elsif( $self->language eq 'English' ) {
		# Do something as yet undetermined
	} else {
		# Latin or Greek or Armenian, just split the chars
		return split( '', $self->code );
	}
};

no Moose;
__PACKAGE__->meta->make_immutable;

1;

