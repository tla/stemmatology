package Text::Tradition::HasStemma;

use strict;
use warnings;
use Moose::Role;
use Text::Tradition::Stemma;
use Text::Tradition::StemmaUtil qw/ parse_newick /;

=head1 NAME

Text::Tradition::HasStemma - add-on to associate stemma hypotheses to
Text::Tradition objects

=head1 DESCRIPTION

It is often the case that, for a given text tradition, the order of copying
of the witnesses can or should be reconstructed (or at least the attempt
should be made.) This class is a role that can be applied to
Text::Tradition objects to record stemma hypotheses.  See the documentation
for L<Text::Tradition::Stemma> for more information.

=head1 METHODS

=head2 stemmata

Return a list of all stemmata associated with the tradition.

=head2 stemma_count

Return the number of stemma hypotheses defined for this tradition.

=head2 stemma( $idx )

Return the L<Text::Tradition::Stemma> object identified by the given index.

=head2 clear_stemmata

Delete all stemma hypotheses associated with this tradition.

=head2 has_stemweb_jobid

Returns true if there is currently a Stemweb job ID, indicating that a
stemma tree calculation from the Stemweb service is in process.

=head2 stemweb_jobid

Return the currently-running job ID (if any) for calculation of Stemweb 
trees.

=head2 set_stemweb_jobid( $jobid )

Record a job ID for a Stemweb calculation.

=cut

has 'stemmata' => (
	traits => ['Array'],
	isa => 'ArrayRef[Text::Tradition::Stemma]',
	handles => {
		stemmata => 'elements',
		_add_stemma => 'push',
		stemma => 'get',
		stemma_count => 'count',
		clear_stemmata => 'clear',
	},
	default => sub { [] },
	);
  
has 'stemweb_jobid' => (
	is => 'ro',
	isa => 'Str',
	writer => 'set_stemweb_jobid',
	predicate => 'has_stemweb_jobid',
	clearer => '_clear_stemweb_jobid',
	);
	
before 'set_stemweb_jobid' => sub {
	my( $self ) = shift;
	if( $self->has_stemweb_jobid ) {
		$self->throw( "Tradition already has a Stemweb jobid: "
			. $self->stemweb_jobid );
	}
};

=head2 add_stemma( $dotfile )

Initializes a Text::Tradition::Stemma object from the given dotfile,
and associates it with the tradition.

=begin testing

use Text::Tradition;

my $t = Text::Tradition->new( 
    'name'  => 'simple test', 
    'input' => 'Tabular',
    'file'  => 't/data/simple.txt',
    );
is( $t->stemma_count, 0, "No stemmas added yet" );
my $s;
ok( $s = $t->add_stemma( dotfile => 't/data/simple.dot' ), "Added a simple stemma" );
is( ref( $s ), 'Text::Tradition::Stemma', "Got a stemma object returned" );
is( $t->stemma_count, 1, "Tradition claims to have a stemma" );
is( $t->stemma(0), $s, "Tradition hands back the right stemma" );

=end testing

=cut

sub add_stemma {
	my $self = shift;
	my %opts = @_;
	my $stemma_fh;
	if( $opts{'dotfile'} ) {
		open $stemma_fh, '<', $opts{'dotfile'}
			or warn "Could not open file " . $opts{'dotfile'};
	} elsif( $opts{'dot'} ) {
		my $str = $opts{'dot'};
		open $stemma_fh, '<', \$str;
	}
	# Assume utf-8
	binmode $stemma_fh, ':utf8';
	my $stemma = Text::Tradition::Stemma->new( 
		'dot' => $stemma_fh );
	$self->_add_stemma( $stemma ) if $stemma;
	return $stemma;
}

=head2 record_stemweb_result( $format, $data )

Records the result returned by a Stemweb calculation, and clears any
existing job ID.

TODO Test!

=cut

sub record_stemweb_result {
	my( $self, $format, $data ) = @_;
	if( $format eq 'dot' ) {
		$self->add_stemma( dot => $data );
	} elsif( $format eq 'newick' ) {
		my $stemmata = parse_newick( $data );
		foreach my $stemma ( @$stemmata ) {
			$self->_add_stemma( $stemma );
		}
		$self->_clear_stemweb_jobid();
	} else {
		$self->throw( "Cannot parse tree results with format $format" );
	}
}

1;

=head1 LICENSE

This package is free software and is provided "as is" without express
or implied warranty.  You can redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHOR

Tara L Andrews E<lt>aurum@cpan.orgE<gt>
