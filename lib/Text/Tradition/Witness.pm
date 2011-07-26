package Text::Tradition::Witness;
use Moose;
use Moose::Util::TypeConstraints;

# Sigil. Required identifier for a witness.
has 'sigil' => (
    is => 'ro',
    isa => 'Str',
    required => 1,
    );

# Text.  This is an array of strings (i.e. word tokens).
# TODO Think about how to handle this for the case of pre-prepared
# collations, where the tokens are in the graph already.
has 'text' => (
    is => 'rw',
    isa => 'ArrayRef[Str]',
    predicate => 'has_text',
    );

# Source.  This is where we read in the witness, if not from a
# pre-prepared collation.  It is probably a filename.
has 'source' => (
    is => 'ro',
    isa => 'Str',
    predicate => 'has_source',
    );

# Path.  This is an array of Reading nodes that should mirror the
# text above.
has 'path' => (
    is => 'rw',
    isa => 'ArrayRef[Text::Tradition::Collation::Reading]',
    predicate => 'has_path',
    );	       

has 'uncorrected_path' => (
    is => 'rw',
    isa => 'ArrayRef[Text::Tradition::Collation::Reading]',
    predicate => 'has_ante_corr',
    );

# Manuscript name or similar
has 'identifier' => (
    is => 'ro',
    isa => 'Str',
    );

# Any other info we have
has 'other_info' => (
    is => 'ro',
    isa => 'Str',
    );
    

sub BUILD {
    my $self = shift;
    if( $self->has_source ) {
	# Read the file and initialize the text.
	my $rc;
	eval { no warnings; $rc = open( WITNESS, $self->source ); };
	# If we didn't open a file, assume it is a string.
	if( $rc ) {
	    my @words;
	    while(<WITNESS>) {
		chomp;
		push( @words, split( /\s+/, $_ ) );
	    }
	    close WITNESS;
	    $self->text( \@words );
	} # else the text is in the source string, probably
	  # XML, and we are doing nothing with it.
    }
}

# If the text is not present, and the path is, and this is a 'get'
# request, generate text from path.
around text => sub {
    my $orig = shift;
    my $self = shift;

    if( $self->has_path && !$self->has_text && !@_ ) {
	my @words = map { $_->label } @{$self->path};
	$self->$orig( \@words );
    }
    
    $self->$orig( @_ );
};

no Moose;
__PACKAGE__->meta->make_immutable;
