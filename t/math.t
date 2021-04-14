use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use PDL::Math;
use Config;

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

ok( tapprox( pow(2,3),8), "pow");
ok( tapprox(erf(0.),0.) && tapprox(erf(30.),1.),"erf(0), erf(30)");
ok( tapprox(erf(0.5),1.-erfc(0.5)), "erf and erfc");
ok( tapprox(erf(erfi(0.5)),0.5) && tapprox(erfi(erf(0.5)),0.5), "erfi (both ways)");

{
my $pa = pdl(0.0,30.0);
$pa->inplace->erf;
ok( tapprox( $pa, pdl(0.0,1.0) ), "erf inplace" );
}

{
my $pa = pdl(0.5);
$pa->inplace->erfc;
ok( tapprox( 1.0-$pa, erf(0.5) ), "erfc inplace" );
}

{
my $pa = pdl( 0.01, 0.0 );
ok( all( approx( erfi($pa), pdl(0.00886,0.0) )), "erfi" );
$pa->inplace->erfi;
ok( all( approx( $pa, pdl(0.00886,0.0) )), "erfi inplace" );
}

ok( all approx( qsort(
	(polyroots(
			pdl( 1,-55,1320,-18150,157773,-902055, 3416930,-8409500,12753576,-10628640,3628800 ),
			zeroes(11)
		))[0]
), 1+sequence(10) ) );

{
my $pa = sequence(41) - 20;
$pa /= 4;
#do test on quarter-integers, to make sure we're not crazy.
my $ans_rint = pdl(-5,-5,-4,-4,-4,-4,-4,-3,-3,-3,-2,-2,-2,-2,-2,
-1,-1,-1,0,0,0,0,0,1,1,1,2,2,2,2,2,3,3,3,4,4,4,4,4,5,5);
ok(all(rint($pa)==$ans_rint),"rint");
}

ok( tapprox(sinh(0.3),0.3045) && tapprox(acosh(42.1),4.43305), "sinh, acosh");
ok( tapprox(acos(0.3),1.2661) && tapprox(tanh(0.4),0.3799), "acos, tanh");
ok( tapprox(cosh(2.0),3.7621) && tapprox(atan(0.6),0.54041), "cosh, atan");

{
# inplace
my $pa = pdl(0.3);
$pa->inplace->sinh;
ok( tapprox($pa, pdl(0.3045)), "sinh inplace" );
}

if ($Config{cc} ne 'cl') {
  # lgamma not implemented for MS compilers
  my @x = lgamma(-0.1);
  is(approx($x[0], 2.36896133272879), 1);
  is($x[1], -1);
  @x = lgamma(1.1);
  is(approx($x[0], -0.0498724412598397), 1);
  is($x[1], 1);
  my $p = sequence (1);
  $p->badvalue (0);
  $p->badflag (1);
  @x = lgamma($p->index(0));
  is($x[0]->badflag(), 1);
  is($x[1]->badflag(), 1);
}

done_testing;
