use strict;
use warnings;
use PDL;
use File::Temp 'tempdir';
use File::Spec;

use Test::More;
use Test::PDL;
use Test::Exception;

# Temp directory name.  The catfile() call adds a trailing dir
# separator (e.g. "/" on POSIX).
my $d = File::Spec->catfile(tempdir(CLEANUP=>1),"");

##1 Make sure the library loads

use PDL::DiskCache;

## Make a DiskCache object
##exercises STORE, sync, and DESTROY

lives_ok {
  my($pa) = diskcache(["${d}1","${d}2","${d}3"],{verbose=>1});
  $pa->[0] = zeroes(10,10);
  $pa->[1] = xvals(10,10);
  $pa->[2] = yvals(10,10);
  1;
} "Make a DiskCache object";

ok( (-e "${d}1") && (-e "${d}2") && (-e "${d}3"), "3 files written");

my $pb;
lives_ok {
  ($pb) = diskcache(["${d}1","${d}2","${d}3"],{ro=>1});
} 'could read files';
ok( ($pb->[0]->sum == 0) && ($pb->[1]->sum == xvals(10,10)->sum), 'files read correctly' );

subtest 'LRU eviction, sync, purge, and custom read/write' => sub {
  my @files = map { "${d}lru_${_}.fits" } 0..4;
  my $cache = diskcache(\@files, { rw => 1, mem => 2, verbose => 0 });
  isa_ok tied(@$cache), 'PDL::DiskCache';
  is scalar(@$cache), 5, 'cache has 5 slots';

  for my $i (0..4) {
    $cache->[$i] = sequence(float, 3, 3) + $i * 10;
  }
  is_pdl $cache->[0], sequence(float, 3, 3), { atol => 0, test_name => 'slot 0 reloaded from disk' };
  is_pdl $cache->[4], sequence(float, 3, 3) + 40, { atol => 0, test_name => 'slot 4 matches' };

  tied(@$cache)->sync;
  ok -f $files[0], 'sync wrote slot 0 to disk';
  tied(@$cache)->purge(1);
  is_pdl $cache->[2], sequence(float, 3, 3) + 20, { atol => 0, test_name => 'slot 2 reloads after purge' };

  my @cfiles = map { "${d}custom_${_}.dat" } 0..1;
  my ($writes, $reads) = (0, 0);
  my $cc = diskcache(\@cfiles, {
    rw    => 1,
    mem   => 1,
    write => sub {
      my ($pdl, $file) = @_;
      $writes++;
      open my $fh, '>', $file or die "open $file: $!";
      print {$fh} join(',', $pdl->list), "\n";
      close $fh;
    },
    read  => sub {
      my ($file) = @_;
      $reads++;
      open my $fh, '<', $file or die "open $file: $!";
      my $line = <$fh>;
      close $fh;
      chomp $line;
      return pdl(split /,/, $line);
    },
  });
  $cc->[0] = pdl(1, 2, 3);
  $cc->[1] = pdl(4, 5, 6);
  tied(@$cc)->sync;
  cmp_ok $writes, '>=', 2, 'custom write callback invoked';
  is_pdl $cc->[0], pdl(1, 2, 3), { atol => 0, test_name => 'custom read callback reloaded slot 0' };
  cmp_ok $reads, '>=', 1, 'custom read callback invoked';
};

done_testing;
