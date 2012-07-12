#!/usr/bin/env perl

use lib 'lib';
use feature 'say';
use strict;
use warnings;
use Getopt::Long;
use Lingua::Features::Structure;
use Text::Tradition::Directory;
use XML::Easy::Syntax qw/ $xml10_name_rx $xml10_namestartchar_rx /;
use TryCatch;

binmode STDOUT, ':utf8';
binmode STDERR, ':utf8';
eval { no warnings; binmode $DB::OUT, ':utf8'; $DB::deep = 1000 };

my( $dbuser, $dbpass );
my $dsn = 'dbi:SQLite:dbname=stemmaweb/db/traditions.db';

GetOptions( 
	'dsn=s'    => \$dsn,
	'u|user=s' => \$dbuser,
	'p|pass=s' => \$dbpass,
	);

my $dbopts = { dsn => $dsn };
$dbopts->{extra_args}->{user} = $dbuser if $dbuser;
$dbopts->{extra_args}->{password} = $dbpass if $dbpass;

my $dir = Text::Tradition::Directory->new( $dbopts );

my $scope = $dir->new_scope();
my $lookfor = $ARGV[0] || '';
foreach my $tinfo ( $dir->traditionlist() ) {
	next unless $tinfo->{'name'} =~ /$lookfor/ || $tinfo->{'id'} eq $lookfor;
	my $tradition = $dir->lookup( $tinfo->{'id'} );
	say STDERR "Found " . $tradition->name;
	my $c = $tradition->collation;
	$c->_set_tradition( $tradition );
	
	# Propagate lexeme forms across transposition links
	foreach my $rel ( $c->relationships ) {
		next unless $c->get_relationship( $rel )->type eq 'transposition';
		my $rdg_a = $c->reading( $rel->[0] );
		my $rdg_b = $c->reading( $rel->[1] );
		if( $rdg_a->disambiguated && !$rdg_b->disambiguated ) {
			propagate_lexemes( $rdg_a, $rdg_b );
		} elsif( $rdg_b->disambiguated && !$rdg_a->disambiguated ) {
			propagate_lexemes( $rdg_b, $rdg_a );
		} elsif( !$rdg_a->disambiguated && !$rdg_b->disambiguated ) {
			say STDERR "Transposition link with nothing disambiguated: @$rel";
		}
	}
		
	
	# Make the changes
	foreach my $rank ( 1 .. $c->end->rank - 1 ) {
		my @rankrdgs = $c->readings_at_rank( $rank );
		# Propagate lexemes and normal forms across spelling / orthographic links
		my %propagated;
		foreach my $r ( @rankrdgs ) {
			next if $propagated{$r->id};
			my @samewords = $c->related_readings( $r, 
				sub { $_[0]->type eq 'spelling' || $_[0]->type eq 'orthographic' } );
			push( @samewords, $r );
			map { $propagated{$_->id} = 1 } @samewords;
			next if @samewords == 1;
			
			my( @haslex, @needslex );
			foreach my $w ( @samewords ) {
				if( $w->disambiguated ) {
					push( @haslex, $w );
				} else {
					push( @needslex, $w );
				}
			}
			# Check that the lexeme forms match for the readings in @haslex
			unless( @haslex ) {
				say STDERR "Multiple same word readings with no disambiguation at rank $rank";
				next;
			}
			my $form;
			my $consistent = 1;
			foreach my $w ( @haslex ) {
				my $wf = join( '//', map { $_->form->to_string } $w->lexemes );
				$form = $wf unless $form;
				unless( $wf eq $form ) {
					warn "Conflicting lexeme on $w at rank $rank";
					$consistent = 0;
				}
			}
			if( $consistent && @haslex ) {
				my $ref = shift @haslex;
				foreach my $w ( @needslex ) {
					propagate_lexemes( $ref, $w );
				}
			}
		}
			
		while( @rankrdgs ) {
			my $r = shift @rankrdgs;
			next if $r->is_meta;
			next if $r->is_nonsense;
			next unless $r->has_lexemes;
			next if grep { !$_->is_disambiguated } $r->lexemes;
			my $rlem = join( ' ', map { $_->form->lemma } $r->lexemes );
			my @rpos = map { $_->form->morphstr } $r->lexemes;
			foreach my $rdg ( @rankrdgs ) {
				next if $r eq $rdg;
				next if $rdg->is_nonsense;
				next unless $rdg->has_lexemes;
				next if grep { !$_->is_disambiguated } $rdg->lexemes;
				next if is_sameword( $c, $r, $rdg );
				# Do the grammatical link if applicable
				my $gram;
				if( join( ' ', map { $_->form->lemma } $rdg->lexemes ) eq $rlem
					&& $rlem !~ /\<unknown\>/ ) {
					say sprintf( "Linking %s (%s) and %s (%s) with grammatical rel",
						$r, $r->text, $rdg, $rdg->text );
					$c->add_relationship( $r, $rdg, { 'type' => 'grammatical' } );
					$gram = 1;
				}
				
				# Do a punctuation link (instead of a lexical link) if applicable
				my $punct;
				if( $rdg->text =~ /^[[:punct:]]$/ && $r->text =~ /^[[:punct:]]$/ ) {
					say sprintf( "Linking %s (%s) and %s (%s) with punctuation rel",
						$r, $r->text, $rdg, $rdg->text );
					$c->add_relationship( $r, $rdg, { 'type' => 'punctuation' } );
					$punct = 1;
				}
				
				# Do the lexical link if applicable
				my @rdgpos = map { $_->form->morphstr } $rdg->lexemes;
				next unless @rpos == @rdgpos;
				my $lex = 1;
				foreach my $i ( 0 .. $#rpos ) {
					my $rst = Lingua::Features::Structure->from_string( $rpos[$i] );
					my $rdgst = Lingua::Features::Structure->from_string( $rdgpos[$i] );
					unless( $rst && $rdgst ) {
						warn "Did not get morph structure from " . 
							$rst ? $rdgpos[$i] : $rpos[$i];
						next;
					}
					unless( $rst->is_compatible( $rdgst ) ) {
						$lex = 0;
					}
				}
				if( $lex && !$punct ) {
					if( $gram ) {
						warn sprintf( "Grammatical link already made for %s (%s) / %s (%s)",
							$r, $r->text, $rdg, $rdg->text );
					} else {
						say sprintf( "Linking %s (%s) and %s (%s) with lexical rel",
							$r, $r->text, $rdg, $rdg->text );
						$c->add_relationship( $r, $rdg, { 'type' => 'lexical' } );
					}
				}
			}
		}
	}

	# Save the lot
	# print $c->as_svg( { nocalc => 1 } );
	$dir->save( $tradition );
}

sub is_sameword {
	my( $c, $rdg1, $rdg2 ) = @_;
	my @samewords = $c->related_readings( $rdg1, 
		sub { $_[0]->type eq 'spelling' || $_[0]->type eq 'orthographic' } );
	my @in_set = grep { $_ eq $rdg2 } @samewords;
	return scalar @in_set;
}

sub propagate_lexemes {
	my( $from, $to ) = @_;
	say sprintf( "Copying lexical form from %s (%s) to %s (%s)",
		$from, $from->text, $to, $to->text );
	$to->normal_form( $from->normal_form );
	$to->_deserialize_lexemes( $from->_serialize_lexemes );
}