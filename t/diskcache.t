use strict;
use warnings;
use PDL;
use PDL::Config;
use File::Temp 'tempdir';
use File::Spec;

# Temp directory name.  The catfile() call adds a trailing dir
# separator (e.g. "/" on POSIX).
my $d = File::Spec->catfile(tempdir(CLEANUP=>1),"");

use Test::More tests => 4;

eval 'use PDL::DiskCache;';
ok( !$@, "use PDL::DiskCache succeeds" ) or diag($@);

## Make a DiskCache object
##exercises STORE, sync, and DESTROY

eval <<'BAR'
  do {
    my($a) = diskcache(["${d}1","${d}2","${d}3"],{verbose=>1});
    $a->[0] = zeroes(10,10);
    $a->[1] = xvals(10,10);
    $a->[2] = yvals(10,10);
  } while(0);
BAR
  ;
ok( !$@, "Make a DiskCache object") or diag($@);

ok( (-e "${d}1") && (-e "${d}2") && (-e "${d}3"), "3 files written");

eval <<'BAZ'
  do {
    my($b) = diskcache(["${d}1","${d}2","${d}3"],{ro=>1});
    ok( ($b->[0]->sum == 0) && ($b->[1]->sum == xvals(10,10)->sum), "files read correctly");
  }
BAZ
  ;
