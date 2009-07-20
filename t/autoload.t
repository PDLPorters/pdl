# -*-perl-*-

# Test PDL::AutoLoader

use strict;

use Test::More;

use PDL::LiteF;

BEGIN {

   if ( ! -f 't/func.pdl' ) {
      plan skip_all => 'This test must be run from ../t';
   }
   else {
      plan tests => 3;
   }

   use_ok('PDL::AutoLoader');

}

use vars qw( @PDLLIB );
$PDL::debug = 1;

@PDLLIB = ("t/"); # this means you have to run the test from ../t

my $x = long(2 + ones(2,2));

my $y = func($x);

ok( (sum($y) == 4*29), 'Check autoload of func.pdl' );

#check that tilde expansion works (not applicable on MS Windows)
SKIP: {
  skip "Inapplicable to MS Windows", 1 if $^O =~ /MSWin/i;
  my $tilde = (PDL::AutoLoader::expand_path('~'))[0];
  my $get = (getpwnam(getpwuid($<)))[7];
  my $echo = qx(echo ~);
  chomp $echo;

is($tilde, $echo, "Check tilde expansion (Got '$get' from (getpwnam(getpwuid(\$<)))[7] )");
}
