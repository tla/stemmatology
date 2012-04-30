#!/usr/bin/perl

use strict;
use warnings;

use Benchmark 'timethis';
use JSON;
use File::Path 'mkpath';

use Text::Tradition;
use Text::Tradition::Directory;
use Test::More 'no_plan';

## Using t/data/besoin.xml  / t/data/besoin.dot as a large test example:
my $test_name = 'besoin';
# my $test_name = 'simple';

## Data file for repeated benchmarks:
my $benchmark_file = 't/data/load-save-benchmark.json';

## SQL file (previously dumped KiokuDB) for testing tradition directory loading:
my $load_sql = 't/data/speed_test_load.sql';

## uuid to load from the above stored db:
my $load_uuid = '7D0AA7C0-92C2-11E1-98B2-D7BDA89F4671';

## Pass the git hash to identify this performance improvement, if you
## want to save the results of this run. Pass nothing to just run a test
## of the current code against the previous best.
my $git_hash = shift;

if($git_hash) {
    diag "Will save results using $git_hash as a key";
} else {
    diag "No git hash passed in, just running test";
}

## Setup
mkpath('t/var') if(!-d 't/var');

my $tradition = Text::Tradition->new(
   'input' => 'Self',
   'file'  => "t/data/${test_name}.xml"
    ## smaller for testing the test!
#    'input' => 'Tabular',
#    'file' => 't/data/simple.txt',
);
$tradition->add_stemma(dotfile => "t/data/${test_name}.dot");

#my $fh = File::Temp->new();
#my $file = $fh->filename;
#$fh->close;
## use t/var so you can look at the results after if neccessary:

my $load_db = 't/var/speed_test_load.db';
unlink($load_db) if(-e $load_db);
my $load_dsn = "dbi:SQLite:dbname=$load_db";
## Prime db from .sql file:
## ?? fails
`sqlite3 $load_db < $load_sql`;

my $save_db = 't/var/speed_test_save.db';
unlink($save_db) if(-e $save_db);
my $save_dsn = "dbi:SQLite:dbname=${save_db}";

my $benchmark_data = load_benchmark($benchmark_file);

my $test_save = sub {
    unlink($save_db) if(-e $save_db);

    my $dir = Text::Tradition::Directory->new(
        dsn => $save_dsn,
        extra_args => { create => 1 },
    );
    ## This seems to be a required magic incantation:
    my $scope = $dir->new_scope;

    ## save the tradition (with stemma) to the db:
    my $uuid = $dir->save($tradition);
#    print STDERR "UUID: $uuid\n";

};

my $test_load = sub {
    my $dir = Text::Tradition::Directory->new(
        dsn => $save_dsn,
    );

    ## This seems to be a required magic incantation:
    my $scope = $dir->new_scope;

    my $tradition = $dir->tradition($load_uuid);
};

my $last_benchmark = $benchmark_data->[-1];
## Benchmark current code:
my $new_save_result = timethis(5, $test_save);

ok($new_save_result->[1] + $new_save_result->[2] < $last_benchmark->{save_times}[1] + $last_benchmark->{save_times}[2], 'Saving to a Tradition Directory got faster');

my $new_load_result = timethis(20, $test_load);

ok($new_load_result->[1] + $new_load_result->[2] < $last_benchmark->{load_times}[1] + $last_benchmark->{load_times}[2], 'Loading from a Tradition Directory got faster');

if($git_hash) {
    push(@{ $benchmark_data }, {
        git_hash => $git_hash,
        load_times => $new_load_result,
        save_times => $new_save_result,
    });

    save_benchmark($benchmark_data);
}

## -----------------------------------------------------------------------------

sub load_benchmark {
    my ($filename) = @_;

    my $loaded_data = [];
    if(-e $filename) {
        local $/;
        open( my $fh, '<', $filename ) || die "$!";
        my $json_text   = <$fh>;
        $fh->close();
        $loaded_data = decode_json( $json_text );
    } else {
        ## bare bones default table:
        $loaded_data = [
            {
                git_hash => '',
                load_times => [1000, 1000, 1000, 0, 0, 5],
                save_times => [1000, 1000, 1000, 0, 0, 5],
            }
        ];
    }

    return $loaded_data;
}

sub save_benchmark {
    my ($filename, $new_benchmarks) = @_;

    my $json_text = encode_json($new_benchmarks);

    open(my $fh, '>', $filename) || die "$!";
    $fh->print($json_text);
    $fh->close();
}
