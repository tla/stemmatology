package Text::Tradition::Witness;
use Moose;
use Moose::Util::TypeConstraints;

subtype 'Correction',
    => as 'ArrayRef',
    => where { return 0 unless @$_ == 3;
	       return 0 unless $_->[0] =~ /^\d+$/;
	       return 0 unless $_->[1] =~ /^\d+$/;
	       foreach my $x ( @{$_->[2]} ) {
		   return $0 unless $x->isa( 'Text::Tradition::Collation::Reading' );
	       }
	       return 1;
	   },
    => message { "Correction must be ref of [ offset, length, replacement_list ]" };

		
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

# Uncorrection.  This is an array of sets of reading nodes that show
# where the witness was corrected.
has 'uncorrected' => (
    is => 'rw',
    isa => 'ArrayRef[Correction]',
    predicate => 'has_uncorrected',
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

sub uncorrected_path {
    my $self = shift;
    my @path;
    push( @path, @{$self->path} );
    foreach my $corr ( @{$self->uncorrected} ) {
	splice( @path, $corr->[0], $corr->[1], @{$corr->[2]} );
    }
    return \@path;
}	

no Moose;
__PACKAGE__->meta->make_immutable;
