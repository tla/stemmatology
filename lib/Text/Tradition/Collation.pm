#!/usr/bin/env perl

package Text::Tradition::Collation;
use Moose;

has 'graph' => (
		is => 'ro',
		isa => 'Text::Tradition::Graph',
		);

# The graph is full of nodes, which have positions and equivalences.
# These have to be stored externally to the graph itself.
has 'positions' => (
		    is => 'ro';
		    isa => 'Text::Tradition::Graph::Position',
		    );

has 'equivalences' => (
		       is => 'rw';
		       isa => 'Text::Tradition::Graph::Equivalence',
		       );

# We need a way to access the parent object.
has 'tradition' => (
		    is => 'ro',
		    isa => 'Text::Tradition',
		    );

# The collation can be created two ways:
# 1. Collate a set of witnesses (with CollateX I guess) and process
#    the results as in 2.
# 2. Read a pre-prepared collation in one of a variety of formats,
#    and make the graph from that.

# The graph itself will (for now) be immutable, and the positions
# within the graph will also be immutable.  We need to calculate those
# positions upon graph construction.  The equivalences between graph
# nodes will be mutable, entirely determined by the user (or possibly
# by some semantic pre-processing provided by the user.)  So the
# constructor should just make an empty equivalences object.  The
# constructor will also need to make the witness objects, if we didn't
# come through option 1.

no Moose;
__PACKAGE__->meta->make_immutable;
