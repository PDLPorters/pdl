# Simple test of the interpol routine

use Test::More tests => 1;
use PDL::Lite;

my $yvalues =  (new PDL( 0..5))   - 20;

my $xvalues = -(new PDL (0..5))*.5;

my $x = new PDL(-2);

ok( $x->interpol($xvalues,$yvalues) == -16 );
