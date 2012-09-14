Text::Tradition - a Perl library for modelling textual variation
===============================================================

This repository contains the Text::Tradition library, developed as part of the [Tree of Texts](http://treeoftexts.arts.kuleuven.be/) project at the KU Leuven.  The library consists of a base distribution (in base/) and three add-on components:

* analysis/ - Code for the representation and analysis of text stemmata.
* morphology/ - Code for the representation of language and morphological information for a text.
* persistence/ - Code for database storage and user access control for Tradition objects.

The base, analysis, and persistence modules are also available on CPAN. The morphology module has dependencies upon several custom or custom-modified libraries, some of which are available in the [cpanmods](https://github.com/tla/cpanmods/) repository, and some of which is not (yet) publicly available. Please contact me if you wish to use it.