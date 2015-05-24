# Simple test of the interpol routine

use Test::More tests => 1;
use PDL::Lite;

use strict;
use warnings;

my $yvalues =  (new PDL( 0..5))   - 20;

my $xvalues = -(new PDL (0..5))*.5;

my $x = new PDL(-2);

is( $x->interpol($xvalues,$yvalues), -16 );
