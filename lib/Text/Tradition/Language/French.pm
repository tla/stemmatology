package Text::Tradition::Language::French;

#use Flemm;
use Text::Tradition::Collation::Reading::WordForm;

=head1 NAME

Text::Tradition::Language::French - language-specific modules for French

=head1 DESCRIPTION

Implements morphology lookup for French words in context.

=head1 SUBROUTINES

=head2 lemmatize( $text )

Evaluates the string using the Flemm package, and returns the results.

=cut

sub lemmatize {
	my $text = shift;
	
	
}

=head2 word_lookup( $word )

Looks up a word using the Flemm package, and returns the possible results.
It is better to use L<lemmatize> for context sensitivity.

=cut

sub word_lookup {
	my $word = shift;
	
}

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews E<lt>aurum@cpan.orgE<gt>
