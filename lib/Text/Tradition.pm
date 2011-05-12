#!/usr/bin/env perl

package Text::Tradition;

use Moose;

has 'collation' => (
		    is => 'ro',
		    isa => 'Text::Tradition::Collation',
		    init_arg => undef,
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
