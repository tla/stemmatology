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
    

sub BUILD {
    my $self = shift;
    if( $self->has_source ) {
	# Read the file and initialize the text.
	open( WITNESS, $self->source ) or die "Could not open " 
	    . $self->file . "for reading";
	# TODO support TEI as well as plaintext, sometime
	my @words;
	while(<WITNESS>) {
	    chomp;
	    push( @words, split( /\s+/, $_ ) );
	}
	close WITNESS;
	$self->text( \@words );
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
