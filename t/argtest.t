# tests for error checking of input args to PP compiled function

use strict;
use warnings;

use Test::More tests => 4;
use Test::Exception;
use PDL::LiteF;
kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

{
	my $pb=pdl([1,2,3])->long;
	my $pa=[1,2,3];
	lives_ok { PDL::Ufunc::sumover($pa,$pb) } 'sumover with piddles of compatible dimensions does not die';
}


{
	my $paa=3;
	my $pa=\$paa;
	throws_ok { PDL::Ufunc::sumover($pa,$paa) } qr/Error - tried to use an unknown/;
}

{
	throws_ok { PDL::Ufunc::sumover({}) } qr/Hash given as a pdl - but not \{PDL} key/;
}

{
	my $pc = 0;
	throws_ok { PDL::Ufunc::sumover(\$pc) } qr/Error - tried to use an unknown/;
}

done_testing;
