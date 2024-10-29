use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use PDL::Math;
use Test::PDL -atol => 0.01;
use Config;

is_pdl bessj0(0.5), pdl(0.9384), "bessj0";
is_pdl bessj0(0), ldouble(1), "bessj0";
is_pdl bessj1(0.1), pdl(0.0499), "bessj1";
is_pdl bessj1(0), ldouble(0) ,"bessj1";
is_pdl bessjn(0.8,3), pdl(0.010), "bessjn";
is_pdl bessyn(0.2,2), pdl(-32.15714) ,"bessyn";

{
# test inplace
my $pa = pdl(0.5,0.0);
$pa->inplace->bessj0;
is_pdl $pa, pdl(0.9384,1), "bessj0 inplace";
eval { $pa->inplace->bessj0(PDL->null) };
isnt $@, '', 'check providing explicit output arg to inplace throws exception';
}

{
my $pa = pdl(0.2);
$pa->inplace->bessyn(2);
is_pdl $pa, pdl(-32.15714), "bessyn inplace";
}

is_pdl pow(2,3), sbyte(8), "pow";
is_pdl erf(0.), pdl(0.),"erf(0)";
is_pdl erf(30.), pdl(1.),"erf(30)";
is_pdl erf(0.5), pdl(1.-erfc(0.5)), "erf and erfc";
is_pdl erf(erfi(0.5)), pdl(0.5), "erfi (both ways)";
is_pdl erfi(erf(0.5)), pdl(0.5), "erfi (both ways)";

{
my $pa = pdl(0.0,30.0);
$pa->inplace->erf;
is_pdl $pa, pdl(0.0,1.0), "erf inplace";
}

{
my $pa = pdl(0.5);
$pa->inplace->erfc;
is_pdl 1.0-$pa, erf(0.5), "erfc inplace";
}

{
my $pa = pdl( 0.01, 0.0 );
is_pdl erfi($pa), my $exp = pdl(0.00886,0.0), "erfi";
$pa->inplace->erfi;
is_pdl $pa, $exp, "erfi inplace";
}

eval {polyroots(1,0)};
like $@, qr/only works/, 'polyroots(1,0) throws exception not segfault';
my $coeffs = pdl(cdouble, 1,-55,1320,-18150,157773,-902055, 3416930,-8409500,12753576,-10628640,3628800);
my $roots = 1+sequence(10);
my $got;
ok all(approx $got=qsort((polyroots $coeffs->re, $coeffs->im)[0]), $roots), 'polyroots' or diag $got;
polyroots $coeffs->re, $coeffs->im, $got=null; $got->inplace->qsort;
ok all(approx $got, $roots), 'polyroots with explicit output args' or diag $got;
ok all(approx $got=qsort(polyroots($coeffs)->re), $roots), 'polyroots native complex no output args' or diag $got;
polyroots $coeffs, $got=null; $got=$got->re->qsort;
ok all(approx $got, $roots), 'polyroots native complex explicit output args' or diag $got;
eval {polyroots(pdl("[1 0 0 0 -1]"),zeroes(5))};
is $@, '', 'polyroots no crash on 4 complex roots of 1';
ok all(approx $got=(polyfromroots $roots, $roots->zeroes)[0], $coeffs->re), 'polyfromroots legacy no outargs' or diag $got;
polyfromroots $roots, $roots->zeroes, $got=null;
ok all(approx $got, $coeffs->re), 'polyfromroots legacy with explicit output args' or diag $got;
ok all(approx $got=polyfromroots(cdouble($roots)), $coeffs->re), 'polyfromroots natcom no outargs' or diag $got;
polyfromroots cdouble($roots), $got=null;
ok all(approx $got, $coeffs), 'polyfromroots natcom explicit outargs' or diag $got;

my ($coeffs2, $x, $exp_val) = (cdouble(3,2,1), cdouble(5,7,9), cdouble(86,162,262));
ok all(approx $got=polyval($coeffs2, $x), $exp_val), 'polyval natcom no output' or diag $got;
polyval($coeffs2, $x, $got=null);
ok all(approx $got, $exp_val), 'polyval natcom explicit output' or diag $got;
ok all(approx $got=(polyval($coeffs2->re, zeroes(3), $x->re, zeroes(3)))[0], $exp_val->re), 'polyval legacy no output' or diag $got;
polyval($coeffs2->re, zeroes(3), $x->re, zeroes(3), $got=null);
ok all(approx $got, $exp_val->re), 'polyval legacy explicit output' or diag $got;

{
my $pa = sequence(41) - 20;
$pa /= 4;
#do test on quarter-integers, to make sure we're not crazy.
my $ans_rint = pdl(-5,-5,-4,-4,-4,-4,-4,-3,-3,-3,-2,-2,-2,-2,-2,
-1,-1,-1,0,0,0,0,0,1,1,1,2,2,2,2,2,3,3,3,4,4,4,4,4,5,5);
ok(all(rint($pa)==$ans_rint),"rint");
}

is_pdl sinh(0.3), pdl(0.3045), "sinh";
is_pdl acosh(42.1), pdl(4.43305), "acosh";
is_pdl acos(0.3), pdl(1.2661), "acos";
is_pdl tanh(0.4), pdl(0.3799), "tanh";
is_pdl cosh(2.0), pdl(3.7621), "cosh";
is_pdl atan(0.6), pdl(0.54041), "atan";

{
# inplace
my $pa = pdl(0.3);
$pa->inplace->sinh;
is_pdl $pa, pdl(0.3045), "sinh inplace";
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
