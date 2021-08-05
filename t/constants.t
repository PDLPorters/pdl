# Simple tests for PDL::Constants

use strict;
use warnings;
use Test::More;
use PDL::Constants qw(PI E DEGRAD);

# just checks values, assumes constant part is ok
ok( abs( PI - 3.14159265358979 ) < 0.0001, 'PI is defined');
ok( abs( E  - 2.71828182845905 ) < 0.0001, 'E  is defined');
ok( abs( DEGRAD - 57.295779513082321 ) < 0.0001, 'DEGRAD is defined');

done_testing();
