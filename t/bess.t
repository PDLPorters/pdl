# -*-perl-*-

use Test::More tests => 6;

use strict;
use warnings;

use PDL::LiteF;
use PDL::Math;

kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub tapprox {
        my($pa,$pb) = @_;
	all approx $pa, $pb, 0.01;
}

ok( tapprox(bessj0(0.5),0.9384) && tapprox(bessj0(0),1) ,"bessj0");
ok( tapprox(bessj1(0.1),0.0499) && tapprox(bessj1(0),0) ,"bessj1");
ok( tapprox(bessjn(0.8,3),0.010) && tapprox(bessyn(0.2,2),-32.15714) ,"bessjn");

{
# test inplace
my $pa = pdl(0.5,0.0);
$pa->inplace->bessj0;
ok( tapprox($pa,pdl(0.9384,1)), "bessj0 inplace" );
}

{
my $pa = pdl(0.2);
$pa->inplace->bessyn(2);
ok( tapprox( $pa, -32.15714 ), "bessyn inplace" );
}

{
ok( tapprox( pow(2,3),8), "pow"); # test for the pow bug
}
