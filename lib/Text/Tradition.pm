package Text::Tradition;

use Module::Load;
use Moose;
use Text::Tradition::Collation;
use Text::Tradition::Witness;

has 'collation' => (
    is => 'ro',
    isa => 'Text::Tradition::Collation',
    writer => '_save_collation',
    );

has 'witnesses' => (
    traits => ['Array'],
    is => 'rw',
    isa => 'ArrayRef[Text::Tradition::Witness]',
    handles => {
	all    => 'elements',
	add    => 'push',
    },
    default => sub { [] },
    );

has 'name' => (
    is => 'rw',
    isa => 'Str',
    default => 'Tradition',
    );

sub BUILD {
    my( $self, $init_args ) = @_;

    if( exists $init_args->{'witnesses'} ) {
	# We got passed an uncollated list of witnesses.  Make a
	# witness object for each witness, and then send them to the
	# collator.
	my $autosigil = 0;
	foreach my $wit ( %{$init_args->{'witnesses'}} ) {
	    # Each item in the list is either a string or an arrayref.
	    # If it's a string, it is a filename; if it's an arrayref,
	    # it is a tuple of 'sigil, file'.  Handle either case.
	    my $args;
	    if( ref( $wit ) eq 'ARRAY' ) {
		$args = { 'sigil' => $wit->[0],
			  'file' => $wit->[1] };
	    } else {
		$args = { 'sigil' => chr( $autosigil+65 ),
			  'file' => $wit };
		$autosigil++;
	    }
	    $self->witnesses->push( Text::Tradition::Witness->new( $args ) );
	    # TODO Now how to collate these?
	}
    } else {
	# Else we need to parse some collation data.  Make a Collation object
	my $collation = Text::Tradition::Collation->new( %$init_args,
							'tradition' => $self );
	$self->_save_collation( $collation );

	# Call the appropriate parser on the given data
	my @formats = grep { /^(GraphML|CSV|CTE|TEI)$/ } keys( %$init_args );
	my $format = shift( @formats );
	unless( $format ) {
	    warn "No data given to create a collation; will initialize an empty one";
	}
	if( $format && $format =~ /^(CSV|CTE)$/ && 
	    !exists $init_args->{'base'} ) {
	    warn "Cannot make a collation from $format without a base text";
	    return;
	}

	# Starting point for all texts
	my $last_node = $collation->add_reading( '#START#' );

	# Now do the parsing. 
	my @sigla;
	if( $format ) {
	    my @parseargs;
	    if( $format =~ /^(CSV|CTE)$/ ) {
		$init_args->{'data'} = $init_args->{$format};
		$init_args->{'format'} = $format;
		$format = 'BaseText';
		@parseargs = %$init_args;
	    } else {
		@parseargs = ( $init_args->{ $format } ); 
	    }
	    my $mod = "Text::Tradition::Parser::$format";
	    load( $mod );
	    $mod->can('parse')->( $self, @parseargs );
	}
    }
}

sub witness {
    my( $self, $sigil ) = @_;
    my $requested_wit;
    foreach my $wit ( @{$self->witnesses} ) {
	$requested_wit = $wit if $wit->sigil eq $sigil;
    }
    # We depend on an undef return value for no such witness.
    # warn "No such witness $sigil" unless $requested_wit;
    return $requested_wit;
}
	

sub add_witness {
    my $self = shift;
    my $new_wit = Text::Tradition::Witness->new( @_ );
    push( @{$self->witnesses}, $new_wit );
    return $new_wit;
}

# The user will usually be instantiating a Tradition object, and
# examining its collation.  The information about the tradition can
# come via several routes:
# - graphML from CollateX or elsewhere, standalone
# - TEI parallel segmentation
# - Leuven-style spreadsheet of variants, converted to CSV, plus base text
# - apparatus pulled from CTE, plus base text
# From this we should be able to get basic witness information.
# 
# Alternatively the user can just give us the uncollated texts.  Then
# instead of passing a collation, s/he is passing a set of witnesses
# from which we will generate a collation.  Those witnesses can be in
# plaintext or in TEI with certain constraints adopted.

# So the constructor for a tradition needs to take one of these infosets,
# and construct the collation and the witness objects.

no Moose;
__PACKAGE__->meta->make_immutable;
