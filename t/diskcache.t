
use strict;

use PDL;
use PDL::Config;
use File::Temp 'tempdir';
use File::Spec;

# Temp directory name.  The catfile() call adds a trailing dir
# separator (e.g. "/" on POSIX).
my $d = File::Spec->catfile(tempdir(CLEANUP=>1),"");

use Test;
BEGIN { plan tests => 4; }

##1 Make sure the library loads

eval 'use PDL::DiskCache;';
if($@) {print $@,"\n";}
ok( !$@ );

##2 Make a DiskCache object


eval <<'BAR'
  do {
    my($a) = diskcache(["${d}1","${d}2","${d}3"],{verbose=>1});
    $a->[0] = zeroes(10,10);
    $a->[1] = xvals(10,10);
    $a->[2] = yvals(10,10);
  } while(0);
BAR
  ;
ok( !$@ );

ok( (-e "${d}1") && (-e "${d}2") && (-e "${d}3") );

eval <<'BAZ'
  do {
    my($b) = diskcache(["${d}1","${d}2","${d}3"],{ro=>1});
    ok( ($b->[0]->sum == 0) && ($b->[1]->sum == xvals(10,10)->sum) );
  }
BAZ
  ;


# end

