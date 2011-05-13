package Text::Tradition::Witness;
use Moose;

# Sigil. Required identifier for a witness.
has 'sigil' => (
		is => 'ro',
		isa => 'Str',
		);

# Text.  This might be an array of strings, but it might also be an
# array of graph nodes.
has 'text' => (
	       is => 'rw',
	       isa => 'Array',
	       );

# File.  This is where we read in the witness, if not from a
# pre-prepared collation.
has 'file' => (
	       is => 'ro',
	       isa => 'Str',
	       );

sub BUILD {
    my $self = shift;
    if( $self->has_file ) {
	# Read the file and initialize the text.
	open( WITNESS, $self->file ) or die "Could not open " 
	    . $self->file . "for reading";
	# TODO support TEI as well as plaintext, sometime
	my @words;
	while(<WITNESS>) {
	    chomp;
	    push( @words, split( /\s+/, $_ ) );
	}
	close WITNESS;
	$self->text( @words );
    }
}

no Moose;
__PACKAGE__->meta->make_immutable;
