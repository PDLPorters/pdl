use strict;
use warnings;
use PDL::Lite;
use PDL::Complex;
use PDL::Math;
use Test::More;

sub tapprox {
	my($x,$y) = @_;
	my $c = abs($x-$y);
	my $d = PDL::max($c);
	$d < 0.0001;
}

# capture error in PDL::LinearAlgebra
{
my $info = bless pdl(1), 'PDL::Complex';
eval { $info ? 1 : 0 };
is $@, '', 'no error in comparison';
}

#Type of cplx and cmplx
my $x=PDL->sequence(2);
my $y=$x->cplx;
is(ref $y, 'PDL::Complex', 'type of cplx');
is(ref $x, 'PDL', "cplx doesn't modify original pdl");
my $z=$y->real;
is(ref $z, 'PDL', 'real returns type to pdl');
is(ref $y, 'PDL::Complex', "real doesn't change type of parent");
#Should there be a real subroutine, such as complex, that does change
#the parent?
$y=$x->complex;
is(ref $y, 'PDL::Complex', 'type of complex');
is(ref $x, 'PDL::Complex', 'complex does modify original pdl');
ok !$x->badflag, 'PDL::Complex badflag works';

eval { my $string = PDL::Complex->null.'' };
is $@, '', 'can stringify complex null';

#Check r2C
is(ref r2C(1), 'PDL::Complex', 'type of r2C');
is(r2C(1)->re, 1, 'real part of r2C');
is(r2C(1)->im, 0, 'imaginary part of r2C');

#Check i2C
is(ref i2C(1), 'PDL::Complex', 'type of i2C');
is(i2C(1)->re, 0, 'real part of i2C');
is(i2C(1)->im, 1, 'imaginary part of i2C');

#Test mixed complex-real operations

my $ref = pdl(-1,1);
$x = i - 1;

is(ref $x, 'PDL::Complex', 'type promotion i - real scalar');
ok(tapprox($x->real,$ref), 'value from i - real scalar') or diag "x=$x, real=", $x->real, "\nexpected: $ref";

$x = 1 - i();
is(ref $x, 'PDL::Complex', 'type promotion real scalar - i');
ok(tapprox($x->real,-$ref), 'value from real scalar - i');

my $native = pdl('[1+2i 3+4i]');
is $native.'', '[1+2i 3+4i]', 'immediate check of native and stringification'
  or diag PDL::Core::pdump($native);
my $fromn = eval { PDL::Complex->from_native($native) };
$ref = pdl([1, 2], [3, 4]);
ok tapprox($fromn->real,$ref), 'from_native works'
  or diag "fromn=$fromn, fromn->real=", $fromn->real, "\nexpected: $ref",
    "native:", PDL::Core::pdump($native), "fromn:", PDL::Core::pdump($fromn);
is $fromn->as_native.'', $native.'', 'as_native'
  or diag "fromn:", PDL::Core::pdump($fromn), "native:", PDL::Core::pdump($native);
is $fromn->as_native->type, 'cdouble', 'as_native right type';

$ref = pdl([[-2,1],[-3,1]]);
$x = i() - pdl(2,3);

is(ref $x, 'PDL::Complex', 'type promotion i - real ndarray');
ok(tapprox($x->real,$ref), 'value from i - real ndarray') or diag "x=$x, real=", $x->real;

$x = pdl(2,3) - i;
is(ref $x, 'PDL::Complex', 'type promotion real ndarray - i');
ok(tapprox($x->real,-$ref), 'value from real ndarray - i');

# dataflow from complex to real
my $ar = $x->real;
$ar++;
ok(tapprox($x->real, -$ref+1), 'complex to real dataflow');

# dataflow from real to complex when using cplx

my $refc=$ref->copy;
my $ac = $refc->cplx;
$ac .= $ac - 1 - i;
ok(tapprox($refc, $ref-1), 'real to complex dataflow') or diag "refc=$refc\nref=$ref\nac=$ac";

# no dataflow from real to complex when complex

$refc=$ref->copy;
$ac = $refc->complex;
$ac .= $ac - 1 - i;
ok(tapprox($refc->real, $ref-1), 'real to complex dataflow');

#Check Cr2p and Cp2r
ok(tapprox(Cr2p(pdl(1,1)), pdl(sqrt(2),atan2(1,1))), 'rectangular to polar');
ok(tapprox(Cp2r(pdl(sqrt(2),atan2(1,1))), pdl(1,1)), 'polar to rectangular');

# Check that converting from re/im to mag/ang and
#  back we get the same thing
$x = cplx($ref);
$y = $x->Cr2p()->Cp2r();
ok(tapprox($x-$y, 0), 'check re/im and mag/ang equivalence');

# Test Cadd, Csub, Cmul, Cscale, Cdiv
$x=1+2*i;
$y=3+4*i;
$a=3;
my $pa=pdl(3);
is(ref Cadd($x,$y), 'PDL::Complex', 'Type of Cadd');
ok(tapprox(Cadd($x,$y)->real, $x->real+$y->real), 'Value of Cadd');
is(ref Csub($x,$y), 'PDL::Complex', 'Type of Csub');
ok(tapprox(Csub($x,$y)->real, $x->real-$y->real), 'Value of Csub');
#is(ref Cmul($x,$y), 'PDL::Complex', 'Type of Cmul');
#ok(tapprox(Cmul($x,$y)->real,
#	   pdl($x->re*$y->re-$x->im*$y->im,
#	       $x->re*$y->im+$x->im*$y->re)), 'Value of Cmul');
is(ref Cscale($x,$a), 'PDL::Complex', 'Type of Cscale with scalar');
ok(tapprox(Cscale($x,$a)->real, $x->real*$a), 'Value of Cscale with scalar');
is(ref Cscale($x,$pa), 'PDL::Complex', 'Type of Cscale with pdl');
ok(tapprox(Cscale($x,$pa)->real, $x->real*$pa), 'Value of Cscale with pdl');
#is(ref Cdiv($x,$y), 'PDL::Complex', 'Type of Cdiv');
#ok(tapprox(Cdiv($x,$y)->real,
#	   Cscale(Cmul($x,$y->Cconj), 1/$y->Cabs2)->real), 'Value of Cdiv');
# to test Cabs, Cabs2, Carg (ref PDL)

ok(tapprox($x->Cconj->im, -2), 'Cconj works');
ok(tapprox($x->conj->im, -2), 'conj works');

$x = cplx($ref);
my $cabs = sqrt($x->re**2+$x->im**2);

is(ref Cabs $x, 'PDL', 'Cabs type');
is(ref Cabs2 $x, 'PDL', 'Cabs2 type');
is(ref Carg $x, 'PDL', 'Carg type');
ok(tapprox($cabs, Cabs $x), 'Cabs value');
ok(tapprox($cabs**2, Cabs2 $x), 'Cabs2 value');
ok(tapprox(atan2($x->im, $x->re), Carg $x), 'Carg value');

#Csin, Ccos, Ctan

is(ref Csin(i), 'PDL::Complex', 'Csin type');
ok(tapprox(Csin($x->re->r2C)->re, sin($x->re)), 'Csin of reals');
ok(tapprox(Csin(i()*$x->im)->im, sinh($x->im)), 'Csin of imags');
is(ref Ccos(i), 'PDL::Complex', 'Ccos type');
ok(tapprox(Ccos($x->re->r2C)->re, cos($x->re)), 'Ccos of reals');
ok(tapprox(Ccos(i()*$x->im)->re, cosh($x->im)), 'Ccos of imags');
is(ref Ctan(i), 'PDL::Complex', 'Ctan type');
ok(tapprox(Ctan($x->re->r2C)->re, tan($x->re)), 'Ctan of reals');
ok(tapprox(Ctan(i()*$x->im)->im, tanh($x->im)), 'Ctan of imags');

#Cexp, Clog, Cpow
is(ref Cexp(i), 'PDL::Complex', 'Cexp type');
ok(tapprox(Cexp($x->re->r2C)->re, exp($x->re)), 'Cexp of reals');
ok(tapprox(Cexp(i()*$x->im->r2C)->real, pdl(cos($x->im), sin($x->im))->mv(1,0)),
	   'Cexp of imags ');
is(ref Clog(i), 'PDL::Complex', 'Clog type');
ok(tapprox(Clog($x)->real,
	   pdl(log($x->Cabs), atan2($x->im, $x->re))->mv(1,0)),
   'Clog of reals');
is(ref Cpow($x, r2C(2)), 'PDL::Complex', 'Cpow type');
ok(tapprox(Cpow($x,r2C(2))->real,
	   pdl($x->re**2-$x->im**2, 2*$x->re*$x->im)->mv(1,0)),
   'Cpow value');

#Csqrt
is(ref Csqrt($x), 'PDL::Complex', 'Csqrt type');
ok(tapprox((Csqrt($x)*Csqrt($x))->real, $x->real), 'Csqrt value');

ok(tapprox(Cpow(i,2)->real, pdl(-1,0)), 'scalar power of i');
ok(tapprox(Cpow(i,pdl(2))->real, pdl(-1,0)), 'real pdl power of i');

#Casin, Cacos, Catan
is(ref Casin($x), 'PDL::Complex', 'Casin type');
ok(tapprox(Csin(Casin($x))->real, $x->real), 'Casin value');
is(ref Cacos($x), 'PDL::Complex', 'Cacos type');
ok(tapprox(Ccos(Cacos($x))->real, $x->real), 'Cacos value');
is(ref Catan($x), 'PDL::Complex', 'Catan type');
ok(tapprox(Ctan(Catan($x))->real, $x->real), 'Catan value');

#Csinh, Ccosh, Ctanh
is(ref Csinh($x), 'PDL::Complex', 'Csinh type');
ok(tapprox(Csinh($x)->real, (i()*Csin($x/i()))->real), 'Csinh value');
is(ref Ccosh($x), 'PDL::Complex', 'Ccosh type');
ok(tapprox(Ccosh($x)->real, (Ccos($x/i()))->real), 'Ccosh value');
is(ref Ctanh($x), 'PDL::Complex', 'Ctanh type');
ok(tapprox(Ctanh($x)->real, (i()*Ctan($x/i()))->real), 'Ctanh value');

#Casinh, Cacosh, Catanh
is(ref Casinh($x), 'PDL::Complex', 'Casinh type');
ok(tapprox(Csinh(Casinh($x))->real, $x->real), 'Casinh value');
is(ref Cacosh($x), 'PDL::Complex', 'Cacosh type');
ok(tapprox(Ccosh(Cacosh($x))->real, $x->real), 'Cacosh value');
is(ref Catanh($x), 'PDL::Complex', 'Catanh type');
ok(tapprox(Ctanh(Catanh($x))->real, $x->real), 'Catanh value');


# Croots

is(ref Croots($x, 5), 'PDL::Complex', 'Croots type');
ok(tapprox(Cpow(Croots($x, 5), r2C(5))->real, $x->real->slice(':,*1')),
   'Croots value');
ok(tapprox(Croots($x, 5)->sumover, pdl(0)),
   'Croots center of mass');

#Check real and imaginary parts
is((2+3*i())->re, 2, 'Real part');
is((2+3*i())->im, 3, 'Imaginary part');

#rCpolynomial
is(ref rCpolynomial(pdl(1,2,3), $x), 'PDL::Complex',
	'rCpolynomial type');
ok(tapprox(rCpolynomial(pdl(1,2,3), $x)->real,
	   (1+2*$x+3*$x**2)->real), 'rCpolynomial value');


# Check cat'ing of PDL::Complex
$y = $x->copy + 1;
my $bigArray = $x->cat($y);
ok(abs($bigArray->sumover->sumover +  8 - 4*i()) < .0001, 'check cat for PDL::Complex');
ok(abs($bigArray->sum() +  8 - 4*i()) < .0001, 'check cat for PDL::Complex');

$z = pdl(0) + i()*pdl(0);
$z **= 2;

ok($z->at(0) == 0 && $z->at(1) == 0, 'check that 0 +0i exponentiates correctly'); # Wasn't always so.

my $zz = $z ** 0;

#Are these results really correct? WLM

ok($zz->at(0) == 1 && $zz->at(1) == 0, 'check that 0+0i ** 0 is 1+0i');

$z **= $z;

ok($z->at(0) == 1 && $z->at(1) == 0, 'check that 0+0i ** 0+0i is 1+0i');

my $r = pdl(-10) + i()*pdl(0);
$r **= 2;
ok($r->at(0) < 100.000000001 && $r->at(0) > 99.999999999 && $r->at(1) == 0,
  'check that imaginary part is exactly zero') or diag "got:$r";

$r = PDL->sequence(2,2,3)->complex;
my $slice = $r->slice('(0),:,(0)');
$slice .= 44;
like $r->slice(':,(1),(0)'), qr/44.*3/ or diag "got:", $r->slice(':,(1),(0)');

$r = r2C(-10);
$r .= 2;
ok(PDL::approx($r->at(0), 2) && PDL::approx($r->at(1), 0),
  'check broadcasting does not make assigning a real value affect imag part') or diag "got:$r";

$r = r2C(2);
$r++;
ok(PDL::approx($r->at(0), 3) && PDL::approx($r->at(1), 0), '++ not imag') or diag "got:$r";

$r = r2C(3);
$r--;
ok(PDL::approx($r->at(0), 2) && PDL::approx($r->at(1), 0), '-- not imag') or diag "got:$r";

#Check Csumover sumover, Cprodover and prodover
$x=PDL->sequence(2,3)+1;
$y=$x->copy->complex;
is(ref $y->Csumover, 'PDL::Complex', 'Type of Csumover');
is($y->Csumover->dim(0), 2, 'Dimension 0 of Csumover');
ok(tapprox($y->Csumover->real, $x->mv(1,0)->sumover),
   'Csumover value');
is(ref $y->sumover, 'PDL::Complex', 'Type of sumover');
is($y->sumover->dim(0), 2, 'Dimension 0 of sumover');
ok(tapprox($y->sumover->real, $x->mv(1,0)->sumover), 'sumover value');
is(ref PDL::sumover($y), 'PDL::Complex', 'Type of sumover');
 TODO: {
     local $TODO="sumover as method and as function differ";
     is(PDL::sumover($y)->dim(0), 2, 'Dimension 0 of sumover');
   SKIP: {
       todo_skip "sumover as function is real sumover", 1;
       ok(tapprox(PDL::sumover($y)->real, $x->mv(1,0)->sumover), 'sumover
	  value');
     }
}

is(ref $y->Cprodover, 'PDL::Complex', 'Type of Cprodover');
is($y->Cprodover->dim(0), 2, 'Dimension 0 of Cprodover');
my @els = map $y->slice(":,($_)"), 0..2;
ok(tapprox($y->Cprodover->real,
	   ($els[0]*$els[1]*$els[2])->real),
	  'Value of Cprodover');
is(ref $y->prodover, 'PDL::Complex', 'Type of prodover');
     is($y->prodover->dim(0), 2, 'Dimension 0 of prodover');
ok(tapprox($y->prodover->real,
	   ($els[0]*$els[1]*$els[2])->real),
   'Value of prodover');


#Check sum
$x=PDL->sequence(2,3)+1;
$y=$x->copy->complex;
is(ref $y->sum, 'PDL::Complex', 'Type of sum');
is($y->sum->dims, 1, 'Dimensions of sum');
is($y->sum->dim(0), 2, 'Dimension 0 of sum');
ok(tapprox($y->sum->real, $x->mv(1,0)->sumover), 'Value of sum');

#Check prod
$x=PDL->sequence(2,3)+1;
$y=$x->copy->complex;
is(ref $y->prod, 'PDL::Complex', 'Type of prod');
is($y->prod->dims, 1, 'Dimensions of prod');
is($y->prod->dim(0), 2, 'Dimension 0 of prod');
ok(tapprox($y->prod->real, $y->prodover->real),
   'Value of prod');

{
# Check stringification of complex ndarray
my $c =  9.1234 + 4.1234*i();
my $dummy = $c->dummy(2,1);
like $dummy.'', qr/9.123\S*4.123/, 'sf.net bug #1176614'
  or diag "c:", PDL::Core::pdump($c), "dummy:", PDL::Core::pdump($dummy);
$c = PDL->sequence(2, 3, 4)->complex;
unlike $c.'', qr/\s\+/, 'stringified no space before +';
}

TODO: {
      local $TODO="Autoincrement creates new copy, so doesn't flow";
      # autoincrement flow
      $x=i;
      $y=$x;
      $y++;
      ok(tapprox($x->real, $y->real), 'autoincrement flow');
      diag("$x should have equaled $y");
}

TODO: {
      local $TODO="Computed assignment creates new copy, so doesn't flow";
      # autoincrement flow
      $x=i;
      $y=$x;
      $y+=1;
      ok(tapprox($x->real, $y->real), 'computed assignment flow');
      diag("$x should have equaled $y");
}
TODO: {
      local $TODO="Computed assignment doesn't modify slices";
      # autoincrement flow
      $x=PDL->sequence(2,3)->complex;
      $y=$x->copy;
      $x+=$x;
      $y->slice('')+=$y;
      ok(tapprox($x->real, $y->real), 'computed assignment to slice');
      diag("$x should have equaled $y");
}

$x=3+4*i;$y=4+2*i; my $c=1+1*i;
ok(Cmul($x,$y) == 4+22*i,"Cmul");
ok($x*$y == 4+22*i,"overloaded *");
ok(Cdiv($x,$y) == 1 + 0.5*i,"Cdiv");
ok($x/$y == 1+0.5*i,"overloaded /");
ok(tapprox(Cabs(atan2(pdl(1)->r2C,pdl(0)->r2C)),PDL::Math::asin(1)),"atan2");

TODO: {
      local $TODO="Transpose of complex data should leave 0-th dimension alone";
      $x=PDL->sequence(2,3,4)->complex;
      $y=$x->transpose;
      is($y->dim(0),2, "Keep dimension 0 when transposing");
}
TODO: {
      local $TODO="complex numbers should not be so after moving dimension 0";

      $x=PDL->sequence(2,2)->complex;
      $y=$x->mv(0,1);
      is(ref $y, 'PDL', 'PDL::Complex becomes real PDL after moving dim 0');
}

#test overloaded operators
{
    my $less = 3-4*i;
    my $equal = -1*(-3+4*i);
    my $more = 3+2*i;
    my $zero_imag = r2C(4);
    eval { my $bool = $less<$more }; ok $@, 'exception on invalid operator';
    eval { my $bool = $less<=$equal }; ok $@, 'exception on invalid operator';
    ok($less==$equal,'equal to');
    ok(!($less!=$equal),'not equal to');
    eval { my $bool = $more>$equal }; ok $@, 'exception on invalid operator';
    eval { my $bool = $more>=$equal }; ok $@, 'exception on invalid operator';
    ok($zero_imag==4,'equal to real');
    ok($zero_imag!=5,'neq real');
}

done_testing;
