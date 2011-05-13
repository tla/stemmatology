package Text::Tradition;

use Text::Tradition::Witness;
use Text::Tradition::Collation;
use Moose;

has 'collation' => (
		    is => 'ro',
		    isa => 'Text::Tradition::Collation',
		    );

has 'witnesses' => (
		    traits => ['Array'],
		    is => 'rw',
		    isa => 'ArrayRef[Text::Tradition::Witness]',
		    handles => {
			all_options    => 'elements',
			add_option     => 'push',
			map_options    => 'map',
			option_count   => 'count',
			sorted_options => 'sort',
		    },
		    );

around BUILDARGS => sub {
    my $orig = shift;
    my $class = shift;

    # Now @_ contains the original constructor args.  Make a
    # collation argument and a witnesses argument.
    my %init_args = @_;
    my %member_objects = { 'collation' => undef,
			   'witnesses' => [] };

    if( exists $init_args{'witnesses'} ) {
	# We got passed an uncollated list of witnesses.  Make a
	# witness object for each witness, and then send them to the
	# collator.
	my $autosigil = 0;
	foreach my $wit ( %{$init_args{'witnesses'}} ) {
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
	    push( @{$member_objects{'witnesses'}},
		  Text::Tradition::Witness->new( $args ) );
	    # Now how to collate these?
	}
    } else {
	$member_objects{'collation'} = 
	    Text::Tradition::Collation->new( %init_args );
	@{$member_objects{'witnesses'}} = 
	    $member_objects->{'collation'}->create_witnesses();
    }

    return $class->$orig( %member_objects );
};

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
