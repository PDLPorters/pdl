no warnings qw(misc);

# Test Script for the PDL interface to the GSL library
#  This tests mainly that the interface is working, i.e. that the
#   functions can be called. 
#  The GSL library already has a extensive test suite, and we
#  do not want to duplicate that effort here.

use PDL;
use Test::More;
	
BEGIN
{
   use PDL::Config;
   if ( $PDL::Config{WITH_GSL} ) {
      eval " use PDL::GSL::DIFF; ";
      unless ($@) {
         plan tests => 4;
      } else {
         plan skip_all => "PDL::GSL::DIFF not installed.";
      }
   } else {
      plan skip_all => "PDL::GSL::DIFF not compiled.";
   }
}

@res = gsldiff(\&myf,1.5);

ok(abs($res[0]- 28.4632075095177) < 1e-6 );

@res = gsldiff(\&myf,1.5,{Method => 'central'});

ok(abs($res[0]- 28.4632075095177) < 1e-6 );

@res = gsldiff(\&myf,1.5,{Method => 'forward'});

ok(abs($res[0]- 28.4632852673531) < 1e-6 );

@res = gsldiff(\&myf,1.5,{Method => 'backward'});

ok(abs($res[0]-28.4631297516823 ) < 1e-6 );


sub myf{
  my ($x) = @_;
  return exp($x**2);
}
