use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use PDL::Math;
use Test::PDL -atol => 0.01;
use Config;
require PDL::Core::Dev;

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

{   # csqrt
  my $pi=4*atan2(1,1);
  my $eiO = exp(i()*(sequence(8)-3)*$pi/4);
  my $eiO2 = exp(i()*(sequence(8)-3)*$pi/8);
  is_pdl csqrt($eiO), $eiO2, "csqrt of complex";
  is_pdl csqrt(-1), i(), "csqrt of real -1";
  my $squares="-9 -4 -1 0 1 4 9";
  my $roots="3i 2i i 0 1 2 3";
  is_pdl long($squares)->csqrt,     cdouble($roots), "csqrt of long";
  is_pdl longlong($squares)->csqrt, cdouble($roots), "csqrt of longlong";
  is_pdl float($squares)->csqrt,    cfloat($roots), "csqrt of float";
  is_pdl double($squares)->csqrt,   cdouble($roots), "csqrt of double";
  is_pdl ldouble($squares)->csqrt,  cldouble($roots), "csqrt of ldouble";
  is_pdl cfloat($squares)->csqrt,   cfloat($roots), "csqrt of cfloat";
  is_pdl cdouble($squares)->csqrt,  cdouble($roots), "csqrt of cdouble";
  is_pdl cldouble($squares)->csqrt, cldouble($roots), "csqrt of cldouble";
  is_pdl pdl('-2i')->csqrt, pdl('1-i');
}

is_pdl cacosh(-1), pdl('3.141592i') if PDL::Core::Dev::got_complex_version('acosh', 1);
is_pdl clog(-1), pdl('3.141592i') if PDL::Core::Dev::got_complex_version('log', 1);
is_pdl cacos(-2), pdl('3.141592-1.316957i') if PDL::Core::Dev::got_complex_version('acos', 1);
is_pdl casin(-2), pdl('-1.570796+1.316957i') if PDL::Core::Dev::got_complex_version('asin', 1);

{   # csqrt_up
  my $pi=4*atan2(1,1);
  my $eiO = exp(i()*sequence(8)*$pi/4);
  my $eiO2 = exp(i()*sequence(8)*$pi/8);
  my $sqrt=csqrt_up($eiO);
  is_pdl($sqrt, $eiO2, "Square of csqrt_up of complex");
  my $i=csqrt_up(-1);
  is_pdl($i, i(), "csqrt_up of real -1");
  my $squares="-9 -4 -1 0 1 4 9";
  my $roots="3i 2i i 0 1 2 3";
  is_pdl long($squares)->csqrt_up,     cdouble($roots), "csqrt_up of long";
  is_pdl longlong($squares)->csqrt_up, cdouble($roots), "csqrt_up of longlong";
  is_pdl float($squares)->csqrt_up,    cfloat($roots), "csqrt_up of float";
  is_pdl double($squares)->csqrt_up,   cdouble($roots), "csqrt_up of double";
  is_pdl ldouble($squares)->csqrt_up,  cldouble($roots), "csqrt_up of ldouble";
  is_pdl cfloat($squares)->csqrt_up,   cfloat($roots), "csqrt_up of cfloat";
  is_pdl cdouble($squares)->csqrt_up,  cdouble($roots), "csqrt_up of cdouble";
  is_pdl cldouble($squares)->csqrt_up, cldouble($roots), "csqrt_up of cldouble";
  is_pdl pdl('-2i')->csqrt_up, pdl('-1+i');
}

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
is_pdl qsort((polyroots $coeffs->re, $coeffs->im)[0]), $roots, 'polyroots';
polyroots $coeffs->re, $coeffs->im, $got=null; $got->inplace->qsort;
is_pdl $got, $roots, 'polyroots with explicit output args';
is_pdl qsort(polyroots($coeffs)->re), $roots, 'polyroots native complex no output args';
polyroots $coeffs, $got=null; $got=$got->re->qsort;
is_pdl $got, $roots, 'polyroots native complex explicit output args';
eval {polyroots(pdl("[1 0 0 0 -1]"),zeroes(5))};
is $@, '', 'polyroots no crash on 4 complex roots of 1';
is_pdl +(polyfromroots $roots, $roots->zeroes)[0], $coeffs->re, 'polyfromroots legacy no outargs';
polyfromroots $roots, $roots->zeroes, $got=null;
is_pdl $got, $coeffs->re, 'polyfromroots legacy with explicit output args';
is_pdl polyfromroots(cdouble($roots)), $coeffs, 'polyfromroots natcom no outargs';
polyfromroots cdouble($roots), $got=null;
is_pdl $got, $coeffs, 'polyfromroots natcom explicit outargs';
eval {pdl("[2 1 0 1]")->r2C->polyroots};
is $@, '', 'polyroots no crash on [2 1 0 1]';

my ($coeffs2, $x, $exp_val) = (cdouble(3,2,1), cdouble(5,7,9), cdouble(86,162,262));
is_pdl polyval($coeffs2, $x), $exp_val, 'polyval natcom no output';
polyval($coeffs2, $x, $got=null);
is_pdl $got, $exp_val, 'polyval natcom explicit output';
is_pdl +(polyval($coeffs2->re, zeroes(3), $x->re, zeroes(3)))[0], $exp_val->re, 'polyval legacy no output';
polyval($coeffs2->re, zeroes(3), $x->re, zeroes(3), $got=null);
is_pdl $got, $exp_val->re, 'polyval legacy explicit output';

{
my $pa = sequence(41) - 20;
$pa /= 4;
#do test on quarter-integers, to make sure we're not crazy.
my $ans_rint = pdl(-5,-5,-4,-4,-4,-4,-4,-3,-3,-3,-2,-2,-2,-2,-2,
-1,-1,-1,0,0,0,0,0,1,1,1,2,2,2,2,2,3,3,3,4,4,4,4,4,5,5);
is_pdl rint($pa), $ans_rint, "rint";
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
  is_pdl $x[0], pdl(2.36896133272879);
  is $x[1], -1;
  @x = lgamma(1.1);
  is_pdl $x[0], pdl(-0.0498724412598397);
  is $x[1], 1;
  my $p = sequence (1);
  $p->badvalue (0);
  $p->badflag (1);
  @x = lgamma($p->index(0));
  is($x[0]->badflag(), 1);
  is($x[1]->badflag(), 1);
}

done_testing;
