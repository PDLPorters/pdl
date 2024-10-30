use strict;
use warnings;
use PDL::LiteF;
use Test::More;
use Test::PDL;
use PDL::Graphics::Limits;
use Config;

*round_pow = \&PDL::Graphics::Limits::round_pow;

my @round_tests =
 ( 
  [ -100, -200, -50 ],
  [ -11, -20, -10 ],
  [ -10, -20, -5 ],
  [ -6, -10, -5 ],
  [ -5, -10, -2 ],
  [ -3, -5, -2 ],
  [ -2, -5, -1 ],
  [ -1   ,  -2   , -0.5   ],
  [ -0.6 ,  -1   , -0.5   ],
  [ -0.5 ,  -1   , -0.2   ],
  [ -0.3 ,  -0.5 , -0.2   ],
  [ -0.2 ,  -0.5 , -0.1   ],
  [ -0.06,  -0.1 , -0.05  ],
  [ -0.05,  -0.1 , -0.02  ],
  [ -0.03,  -0.05, -0.02  ],
  [ -0.02,  -0.05, -0.01  ],

  [ 0, 0, 0 ],
  [ 0.02, 0.01, 0.05 ],
  [ 0.03, 0.02, 0.05 ],
  [ 0.05, 0.02, 0.1 ],
  [ 0.06, 0.05, 0.1 ],
  [ 0.2, 0.1, 0.5 ],
  [ 0.3, 0.2, 0.5 ],
  [ 0.5, 0.2, 1 ],
  [ 0.6, 0.5, 1 ],
  [ 1, 0.5, 2 ],
  [ 2, 1, 5 ],
  [ 3, 2, 5 ],
  [ 5, 2, 10 ],
  [ 6, 5, 10 ],
  [ 10, 5, 20 ],
  [ 11, 10, 20 ],
  [ 100, 50, 200 ],

  $Config{uselongdouble} ? () : (
  # these fail on some platforms with NV=long double
  [ -0.1 ,  -0.2 , -0.05  ],
  [ -0.01,  -0.02, -0.005 ],
  [ 0.01, 0.005, 0.02 ],
  [ 0.1, 0.05, 0.2 ],
  ),
 );

for my $test ( @round_tests )
{
  my $down = pdl round_pow( down => $test->[0] );
  my $up   = pdl round_pow( up   => $test->[0] );
  is_pdl $down, pdl($test->[1]), "round_pow($test->[0])";
  is_pdl $up, pdl($test->[2]), "round_pow($test->[0])";
}

done_testing;
