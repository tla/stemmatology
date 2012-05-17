package Text::Tradition::Language::English;

use strict;
use warnings;
use Text::Tradition::Language::Base qw/ lemmatize_treetagger reading_lookup_treetagger /;
use TryCatch;

=head1 NAME

Text::Tradition::Language::English - language-specific module for English

=head1 DESCRIPTION

Implements morphology lookup for English words in context.  This module
depends on the TreeTagger software
(L<http://www.ims.uni-stuttgart.de/projekte/corplex/TreeTagger/>), which is
(for now) expected to be installed in $MORPHDIR/TreeTagger.

=head1 SUBROUTINES

=head2 lemmatize( $text )

Evaluates the string using the TreeTagger, and returns the results.

=begin testing

binmode STDOUT, ':utf8';
use Text::Tradition;
use_ok( 'Text::Tradition::Language::English' );

=end testing

=cut

sub lemmatize {
	my $tradition = shift;
	my %opts = ( 
		'language' => 'English', 
		'callback' => sub { _parse_wordform( @_ ) } 
		);
	return lemmatize_treetagger( $tradition, %opts );
}

=head2 reading_lookup( $rdg[, $rdg, ...] )

Looks up one or more readings using the Flemm package, and returns the
possible results.  This uses the same logic as L<lemmatize> above for the
entire tradition, but can also be used to (re-)analyze individual readings.

=cut

sub reading_lookup {
	my( @path ) = @_;
	my %opts = ( 
		'language' => 'French',
		'callback' => sub { _parse_wordform( @_ ) },
		'path' => \@path,
		);
	return reading_lookup_treetagger( %opts );
}

# Utility function to turn a TreeTagger result into a WordForm
sub _parse_wordform {
	my $tagresult = shift;
	my( $orig, $tag, $lemma ) = split( /\t/, $tagresult );
	my $morphobj = Lingua::TagSet::TreeTagger->tag2structure( $tag );
	if( $morphobj ) {
		return Text::Tradition::Collation::Reading::WordForm->new(
			'language' => 'English',
			'lemma' => $lemma,
			'morphology' => $morphobj,
			);
	} else {
		warn "No morphology found for word: $_";
	}
}

1;

=head2 TODO

=over

=item * Tests!

=back

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews E<lt>aurum@cpan.orgE<gt>
