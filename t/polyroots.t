use Test::More tests => 1;
use PDL::LiteF;
use PDL::Math;

use strict;
use warnings;

kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

ok( all approx( qsort(
	(polyroots(
			pdl( 1,-55,1320,-18150,157773,-902055, 3416930,-8409500,12753576,-10628640,3628800 ),
			zeroes(11)
		))[0]
), 1+sequence(10) ) );
