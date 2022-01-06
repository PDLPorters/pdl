# Test PDL::AutoLoader
use strict;
use warnings;
use Test::More;
use PDL::LiteF;

plan skip_all => 'This test must be run from t/..' if !-f 't/func.pdl';

use_ok('PDL::AutoLoader');

#$PDL::debug = 1;

our @PDLLIB = ("./t"); # this means you have to run the test from t/..

my $x = long(2 + ones(2,2));

my $y = func($x);

ok( (sum($y) == 4*29), 'Check autoload of func.pdl' );

#check that tilde expansion works (not applicable on MS Windows)
SKIP: {
   skip "Inapplicable to MS Windows", 1 if $^O =~ /MSWin/i;
   my $tilde = (PDL::AutoLoader::expand_path('~'))[0];
   my $get = $ENV{'HOME'} || (getpwnam( getlogin || getpwuid($<) ))[7];
   my $glob = glob q(~);
   if ($glob !~ /^~/) {
      is($tilde, $glob, "Check tilde expansion (Got '$get' from (getpwnam(getpwuid(\$<)))[7] )");
   } else {
      is($tilde, $get, "Check tilde expansion (Got '$glob' from glob ~");
   }
}

done_testing;
