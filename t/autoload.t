# -*-perl-*-

# Test PDL::AutoLoader

use strict;

use Test::More;

use PDL::LiteF;

BEGIN {

   if ( ! -f 't/func.pdl' ) {
      plan skip_all => 'This test must be run from ../t';
   }
   elsif ($^O =~ /mswin/i) {
      plan tests => 2;
   }
   else {
      plan tests => 3;
   }

   use_ok('PDL::AutoLoader');

}

use vars qw( @PDLLIB );

@PDLLIB = ("t/"); # this means you have to run the test from ../t

my $x = long(2 + ones(2,2));

my $y = func($x);

ok( (sum($y) == 4*29), 'Check autoload of func.pdl' );

#check that tilde expansion works (not applicable on MS Windows)
my ($tilde, $get, $echo);
unless($^O =~ /mswin/i) {
   $tilde = (PDL::AutoLoader::expand_path('~'))[0];
   $get = (getpwuid($>))[7];
   $echo = qx(echo ~);
   chomp $echo;
}

is($tilde, $get, "Check tilde expansion ($echo from 'echo ~')");
