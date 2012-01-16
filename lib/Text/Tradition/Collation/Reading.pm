package Text::Tradition::Collation::Reading;

use Moose;
use overload '""' => \&_stringify, 'fallback' => 1;

=head1 NAME

Text::Tradition::Collation::Reading - represents a reading (usually a word) in a collation.
    
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

=item collation - The Text::Tradition::Collation object to which this reading belongs.  Required.

=item id - A unique identifier for this reading. Required.

=item text - The word or other text of the reading.

=item is_start - The reading is the starting point for the collation.

=item is_end - The reading is the ending point for the collation.

=item is_lacuna - The 'reading' represents a known gap in the text.

=item is_ph - A temporary placeholder for apparatus parsing purposes.  Do not use unless you know what you are doing.

=item rank - The sequence number of the reading. This should probably not be set manually.

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
	
has 'punctuation' => (
	traits => ['Array'],
	isa => 'ArrayRef[HashRef[Str]]',
	default => sub { [] },
	handles => {
			punctuation => 'elements',
			add_punctuation => 'push',
			},
	);

has 'separate_punctuation' => (
	is => 'ro',
	isa => 'Bool',
	default => 1,
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

has 'rank' => (
    is => 'rw',
    isa => 'Int',
    predicate => 'has_rank',
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
		$args->{'text'} = sprintf( "#LACUNA_%s#", $args->{'id'} );
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

# Post-process the given text, stripping punctuation if we are asked.
sub BUILD {
	my $self = shift;
	if( $self->separate_punctuation && !$self->is_meta ) {
		my $pos = 0;
		my $wspunct = '';  # word sans punctuation
		foreach my $char ( split( //, $self->text ) ) {
			if( $char =~ /^[[:punct:]]$/ ) {
				$self->add_punctuation( { 'char' => $char, 'pos' => $pos } );
			} else {
				$wspunct .= $char;
			}
			$pos++;
		}
		$self->alter_text( $wspunct );
	}
}

sub punctuated_form {
	my $self = shift;
	my $word = $self->text;
	foreach my $p ( sort { $a->{pos} <=> $b->{pos} } $self->punctuation ) {
		substr( $word, $p->{pos}, 0, $p->{char} );
	}
	return $word;
}

=head2 is_meta

A meta attribute (ha ha), which should be true if any of our 'special'
booleans are true.  Implies that the reading does not represent a bit 
of text found in a witness.

=cut

sub is_meta {
	my $self = shift;
	return $self->is_start || $self->is_end || $self->is_lacuna || $self->is_ph;	
}

# Some syntactic sugar
sub related_readings {
	my $self = shift;
	return $self->collation->related_readings( $self, @_ );
}

sub predecessors {
	my $self = shift;
	my @pred = $self->collation->sequence->predecessors( $self->id );
	return map { $self->collation->reading( $_ ) } @pred;
}

sub successors {
	my $self = shift;
	my @succ = $self->collation->sequence->successors( $self->id );
	return map { $self->collation->reading( $_ ) } @succ;
}

sub set_identical {
	my( $self, $other ) = @_;
	return $self->collation->add_relationship( $self, $other, 
		{ 'type' => 'transposition' } );
}

sub _stringify {
	my $self = shift;
	return $self->id;
}

no Moose;
__PACKAGE__->meta->make_immutable;

1;

