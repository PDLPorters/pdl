# -*-perl-*-
#

use strict;
use Test::More;

plan tests => 74    # everything else
              + 27  # nnslice
              + 52  # copied slice tests to nnslice
    ;
use PDL::LiteF;

# PDL::Core::set_debugging(1);

# Useful for debugging. Removed by DJB whilst cleaning up the
# tests
#
#sub kill_if_debug () {
#    kill INT,$$  if $ENV{UNDER_DEBUGGER};
#}

sub tapprox ($$) {
    my $a = shift;
    my $b = shift;
    my $maxdiff = abs($a-$b)->max;
    return $maxdiff < 0.01;
}

my ($a, $b, $c, $d, $e, $f);

$a = (1+(xvals zeroes 4,5) + 10*(yvals zeroes 4,5));

is($a->at(2,2), 23, "a location (2,2) is 23");

$b = $a->slice('1:3:2,2:4:2');

is($b->at(0,0), 22, "(1,2)->(0,0)");
is($b->at(1,0), 24, "(3,2)->(1,0)");
is($b->at(0,1), 42, "(1,4)->(0,1)");
is($b->at(1,1), 44, "(3,4)->(1,1)");

$b .= 0.5 * ones(2,2);

is($b->at(1,0), 0.5,);
is($b->at(0,1), 0.5,);

is($a->at(1,2), 0.5);

# Check that nothing happened to other elems
is($a->at(2,2), 23);

$a = pdl (1,2);
$b = pdl [[1,2],[1,2],[1,2]];
$c = $a->slice(',*3');

# check dimensions, sum of elements and correct order of els (using tapprox)

my $sum;
# $c = $a->dummy(1,3);
sumover($c->clump(-1),($sum=null));
ok(tapprox($b,$c));
is($sum->at, 9);

is(join(',',$c->dims), "2,3");

$b = pdl [[1,1,1],[2,2,2]];
$c = $a->slice('*3,');
sumover($c->clump(-1),($sum=null));

ok(tapprox($b,$c));
is($sum->at, 9);
is(join(',',$c->dims), "3,2");

# test stringify
$a = zeroes(3,3);
my $line = $a->slice(':,(0)');

$a++;
# $line += 0; # that's how to force an update before interpolation
my $linepr = "$line";

is($linepr, '[1 1 1]');

# Test whether error is properly returned:

$b = zeroes(5,3,3);
$c = $b->slice(":,:,1");

is(join(',',$c->dims), "5,3,1");

eval { my $d = $c->slice(":,:,2"); " $d" };
like($@, qr/(out of bounds)|(Slice cannot start or end)/, 'check slice bounds error handling');

$a = zeroes 3,3;
$b = $a->slice("1,1:2");

$b .= 1;

$a = xvals zeroes 20,20;

$b = $a->slice("1:18:2,:");
$c = $b->slice(":,1:18:2");
$d = $c->slice("3:5,:");
$e = $d->slice(":,(0)");
$f = $d->slice(":,(1)");

is("$e", "[7 9 11]");
is("$f", "[7 9 11]");

# Make sure that vaffining is properly working:

$a = zeroes 5,6,2;

$b = (xvals $a) + 0.1 * (yvals $a) + 0.01 * (zvals $a);

$b = $b->copy;

$c = $b->slice("2:3");

$d = $c->copy;

# $c->dump;
# $d->dump;

$e = $c-$d;


is(max(abs($e)), 0);

use PDL::Dbg;

my ($im, $im1, $im2, $lut, $in);

$im = byte [[0,1,255],[0,0,0],[1,1,1]];
($im1 = null) .= $im->dummy(0,3);

$im2 = $im1->clump(2)->slice(':,0:2')->px;

ok(!tapprox(ones(byte,9,3),$im2));

# here we encounter the problem
$im2 = $im1->clump(2)->slice(':,-1:0')->px;
ok(!tapprox(ones(byte,9,3),$im2));

$a = xvals( zeroes 10,10) + 0.1*yvals(zeroes 10,10);
ok(tapprox($a->mslice('X',[6,7]),
	   pdl([
		[0.6, 1.6, 2.6, 3.6, 4.6, 5.6, 6.6, 7.6, 8.6, 9.6],
		[0.7, 1.7, 2.7, 3.7, 4.7, 5.7, 6.7, 7.7, 8.7, 9.7]
	       ])));

$lut = pdl [[1,0],[0,1]];
$im = pdl [1];
$in = $lut->xchg(0,1)->index($im->dummy(0));

is("$in", "
[
 [0 1]
]
");

$in .= pdl 1;

is("$in", "
[
 [1 1]
]
");
ok(tapprox($lut,pdl([[1,0],[1,1]])));

# can we catch indices which are too negative?
$a = PDL->sequence(10);
$b = $a->slice('0:-10');
is("$b", "[0]", "slice 0:-n picks first element");

$b = $a->slice('0:-14');
eval '"$b"';
like($@, qr/(out of bounds)|(Negative slice cannot start or end above limit)/);

# Test of dice and dice_axis
$a = sequence(10,4);
is($a->dice([1,2],[0,3])->sum, 66, "dice");
is($a->dice([0,1],'X')->sum, 124, "dice 'X'");

# Test of Reorder:
$a = sequence(5,3,2);
my @newDimOrder = (2,1,0);
$b = $a->reorder(@newDimOrder);

# since doing floating-point arithmetic here, should probably
# use a better test than "eq" here
#
is($b->average->average->sum , 72.5, "Test of reorder");

$a = zeroes(3,4);
$b = $a->dummy(-1,2);
is(join(',',$b->dims), '3,4,2');

$a = pdl(2);
$b = $a->slice('');
ok(tapprox($a, $b), "Empty slice");

$a = pdl [1,1,1,3,3,4,4,1,1,2];
$b = null;
$c = null;
rle($a,$b,$c);
ok(tapprox($a, rld($b,$c)));

$b = $a->mslice(0.5);
ok(tapprox($b, 1), "mslice 1");

$b = $a->mslice([0.5,2.11]);
is("$b", "[1 1 1]", "mslice 2");

$a = zeroes(3,3);
$b = $a->splitdim(3,3);
eval '$b->make_physdims';
like($@, qr/^Splitdim: nthdim/, "make_physdim: Splitdim");

$a = sequence 5,5;
$b = $a->diagonal(0,1);
is("$b", "[0 6 12 18 24]", "diagonal");

$a = sequence 10;
eval '$b = $a->lags(1,1,1)->make_physdims';
like($@, qr/lags: dim out of range/, "make_physdim: out of range");

eval '$b = $a->lags(0,-1,1)->make_physdims';
like($@, qr/lags: step must be positive/, "make_physdim: negative step");

eval '$b = $a->lags(0,1,11)->make_physdims';
like($@, qr/too large/, "make_pyhsdim: too large");

##############################
# Tests of permissive slicing and dummying

$a = xvals(5,5)+10*yvals(5,5);

eval '$b = $a->slice("1,2,(0)")->make_physical';
ok(!$@);
is($b->ndims, 2, "slice->make_physical: ndims");
is(pdl($b->dims)->sumover, 2, "slice->make_physical: dims");

eval '$c = $a->slice("1,2,(1)")->make_physical';
like($@, qr/too many dims/i, "slice->make_physical: too many dims");

# Hmmm, think these could be split up but not sure exactly what is being
# tested so leave as is (ish)
#
eval '$d = $a->slice("0:1,2:3,0")->make_physical';
ok(!$@);
ok(eval '$d->ndims == 3 && ((pdl($d->dims) == pdl(2,2,1))->sumover == 3)' && !$@);

eval '$d = $a->slice("0:1,2:3,0")->xchg(0,2)';
ok(!$@, "slice->xchg");

ok(eval '$d->ndims == 3 && ((pdl($d->dims) == pdl(1,2,2))->sumover == 3)' && !$@);

eval '$e = $a->dummy(6,2)';
ok(!$@, "dummy");

ok(eval '$e->ndims == 7 && ((pdl($e->dims) == pdl(5,5,1,1,1,1,2))->sumover==7)' && !$@);

##############################
# Tests of indexND (Nowadays this is just another call to range)

my ($source, $index, $dest, $z);

# Basic indexND operation
$source = 10*xvals(10,10) + yvals(10,10);
$index  = pdl([[2,3],[4,5]],[[6,7],[8,9]]);
eval '$a = $source->indexND( $index )';
ok(!$@);
ok(eval 'zcheck($a != pdl([23,45],[67,89]))', "eval of zcheck 1");

# Threaded indexND operation
$source = 100*xvals(10,10,2)+10*yvals(10,10,2)+zvals(10,10,2);
$index  = pdl([[2,3],[4,5]],[[6,7],[8,9]]);
eval '$a = $source->indexND($index)';
ok(!$@);
ok(eval 'zcheck($a != pdl([[230,450],[670,890]],[[231,451],[671,891]]))', "eval of zcheck 2");


##############################
# Tests of range operator

# Basic range operation
$source = 10*xvals(10,10) + yvals(10,10);
$index = pdl([[2,3],[4,5]],[[6,7],[8,9]]);

eval '$dest = $source->range($index);';
ok(!$@);
ok(eval 'zcheck($dest != pdl([23,45],[67,89]));', "eval of zcheck 3");

# Make a 3x3 range at each index
eval '$dest = $source->range($index,3);';
ok(!$@);

# Check that the range has the correct size
is($dest->ndims, 4, "ndims after range");
ok(zcheck(pdl($dest->dims) != pdl(2,2,3,3)), "zcheck after range");

#### Check boundary conditions
eval '$z = $dest->copy;'; # Should throw range-out-of-bounds error
ok($@); # should check actual error message here

## Truncation
eval '$z = $source->range($index,3,"t")->copy;';
ok(!$@);  # Should NOT throw range-out-of-bounds error.
ok(zcheck($z->slice("(1),(1)") != pdl([[89,99,0],[0,0,0],[0,0,0]])));

## Truncation on one axis, periodic on another; string syntax
eval '$z = $source->range($index,3,"tp")';
ok(zcheck($z->slice("(1),(1)") != pdl([[89,99,0],[80,90,0],[81,91,0]])));

## Periodic on first axis, extension on another; list syntax
eval '$z = $source->range($index,3,["e","p"]);';
ok(zcheck($z->slice("(1),(1)") != pdl([[89,99,99],[80,90,90],[81,91,91]])));

our $mt;
eval 'our $mt = which(pdl(0))';
ok("$mt" =~ m/^Empty/);

our $dex = pdl(5,4,3);
$z = $dex->range(zeroes(0));  # scalar Empties are autopromoted like scalar nonempties
ok("$z" eq 'Empty[0]', "scalar Empty[0] indices handled correctly by range");

$z = $dex->range(zeroes(1,0)); # 1-vector Empties are handled right.
ok("$z" eq 'Empty[0]', "1-vector Empty[1,0] indices handled correctly by range");


$z = $mt->range($dex,undef,'e');
ok(all($z==0),"empty source arrays handled correctly by range");

$z = $mt->range($mt);
ok("$z" eq 'Empty[0]', "ranging an empty array with an empty index gives Empty[0]");

$a = pdl(5,5,5,5);
$z = $a->range($mt);
ok("$z" eq 'Empty[0]');

$z .= 2;
ok(1);            # should *not* segfault!
ok(all($a==5));   # should *not* change $a!


######################################################################
######################################################################
##  nnslice tests -- work it out

$a = sequence(10,10);
eval { $b = $a->nnslice(); };  # should not segfault
ok(1, 'nnsliceb MakeComps does not segfault for simple case');
ok(!$@, 'nnsliceb does not throw an error for simple case');

eval { $c = $b->at(0,0); }; # force redodims
ok(1, 'nnsliceb RedoDims does not segfault for simple case');
ok(!$@, 'nnsliceb does not throw an error on redodims');

ok( ($a->ndims == $b->ndims) && (all($a->shape==$b->shape)), 'trivial nnslice duplicates array');

$b = $a->nnslice([2],[3,'X']); # column slice one way; squish the other
ok( ($b->ndims==1) && ($b->dim(0)==1), 'column-pick / squish yields right shape');
ok( $b==32 , 'column-pick / squish extracts the right value');

$b = $a->nnslice([2,4],['X']); # three-column slice one way; full-column the other
ok( ($b->ndims==2) && all($b->shape==pdl(3,10)), 'three-column / full-column has the right shape');
ok( all($b== 2 + xvals(3,10) + 10*yvals(3,10)), 'three-column / full-column slice gets the right values');

$b = $a->nnslice([2,4,0]); # alternate way to squish a dim -- element 1 (4) is ignored
ok( ($b->ndims==1) && ($b->dim(0)==10), 'alternate squish works' );
ok( all($b==2+10*xvals(10)), 'squish works okay');

$b = $a->nnslice([4,2],[2,4,2]); # descending three-column slice one way; alternate two-column slice the other
ok( ($b->ndims==2) && ($b->dim(0)==3) && ($b->dim(1)==2), 'descending/alternating slice has the right shape');
ok( all($b==4-xvals(3,2) + 20 + yvals(3,2)*20), 'descending/alternating slice gets the right values');

$b = $a->nnslice([4,4,1],[4,2,1]); # check weird specified cases -- 1x0 empty
ok( ($b->ndims==2) && ($b->dim(0)==1) && ($b->dim(1)==0) , '1x0 extended empty');

$b = $a->nnslice([2],[3],['*',5]); # dummy dimension
ok( ($b->ndims==3) && all($b->shape==pdl(1,1,5)), 'dummy dimension works');
ok( all($b==32), 'dummy dimension works for content' );

$b = $a->nnslice([2],[3],['*9',5,7]); # everything after the '*' and in element 2 should be ignored
ok( ($b->ndims==3) && all($b->shape==pdl(1,1,5)), 'dummy dimension works if abused');

$b = $a->nnslice(['*'],[3],[4]);
ok( ($b->ndims==3) && all($b->shape==pdl(1,1,1)), 'default dummy dim works');
ok( $b==43, 'default dummy dims gets correct values');

$b = $a->nnslice([0,-8],[-4,5]); # resolve negatives (resolves to [0,2] and [6,5]);
ok( ($b->ndims==2) && all($b->shape==pdl(3,2)), 'negative counts work okay' );
ok( all( $b== xvals(3,2) + 60 - yvals(3,2)*10 ), 'negative counts get right values');

# out-of-bounds checks
eval { $b = $a->nnslice([10,3]); };
ok(!$@, 'out-of-bounds slice succeeds right away');
eval { "$b" };
ok($@, "out-of-bounds slice fails on eval");

eval { $b = $a->nnslice([-11,3]); };
ok(!$@, "negative oob slice succeeds");
eval { "$b" };
ok($@, "negative out-of-bounds fails on eval");

$b = $a->nnslice([-1,0,0]); # squish X, select last element
ok( ($b->ndims==1) && ($b->dim(0)==10), "squish gives correct dimensions");
ok( all($b == 9 + 10*xvals(10)), "squished regularized dim gives correct answer");


###
###

##  Copy of slice tests for nnslice... ced

my ($a, $b, $c, $d, $e, $f);

$a = (1+(xvals zeroes 4,5) + 10*(yvals zeroes 4,5));


is($a->at(2,2), 23);

$b = $a->nnslice('1:3:2,2:4:2');

is($b->at(0,0), 22);
is($b->at(1,0), 24);
is($b->at(0,1), 42);
is($b->at(1,1), 44);

$b .= 0.5 * ones(2,2);

is($b->at(1,0), 0.5);
is($b->at(0,1), 0.5);

is($a->at(1,2), 0.5);

# Check that nothing happened to other elems
is($a->at(2,2), 23);

$a = pdl (1,2);
$b = pdl [[1,2],[1,2],[1,2]];
$c = $a->nnslice(',*3');

# check dimensions, sum of elements and correct order of els (using tapprox)

my $sum;
# $c = $a->dummy(1,3);
sumover($c->clump(-1),($sum=null));
ok(tapprox($b,$c));
is($sum->at, 9);

is(join(',',$c->dims), "2,3");

$b = pdl [[1,1,1],[2,2,2]];
$c = $a->nnslice('*3,');
sumover($c->clump(-1),($sum=null));

ok(tapprox($b,$c));
is($sum->at, 9);
is(join(',',$c->dims), "3,2");

# test stringify
$a = zeroes(3,3);
my $line = $a->nnslice(':,(0)');

$a++;
# $line += 0; # that's how to force an update before interpolation
my $linepr = "$line";

is($linepr, '[1 1 1]');

# Test whether error is properly returned:

$b = zeroes(5,3,3);
$c = $b->nnslice(":,:,1");

is(join(',',$c->dims), "5,3,1");

eval { my $d = $c->nnslice(":,:,2"); "$d"; };

like($@, qr/(out of bounds)|(Slice cannot start or end)/, 'check slice bounds error handling');

$a = zeroes 3,3;

$b = $a->nnslice("1,1:2");


$b .= 1;

$a = xvals zeroes 20,20;

$b = $a->nnslice("1:18:2,:");
$c = $b->nnslice(":,1:18:2");
$d = $c->nnslice("3:5,:");
$e = $d->nnslice(":,(0)");
$f = $d->nnslice(":,(1)");


is("$e", "[7 9 11]");
is("$f", "[7 9 11]");

# Make sure that vaffining is properly working:

$a = zeroes 5,6,2;

$b = (xvals $a) + 0.1 * (yvals $a) + 0.01 * (zvals $a);

$b = $b->copy;

$c = $b->nnslice("2:3");

$d = $c->copy;

# $c->dump;
# $d->dump;

$e = $c-$d;

# $c->dump; $d->dump;

is(max(abs($e)), 0);

use PDL::Dbg;

my ($im, $im1, $im2, $lut, $in);

$im = byte [[0,1,255],[0,0,0],[1,1,1]];
($im1 = null) .= $im->dummy(0,3);
$im2 = $im1->clump(2)->nnslice(':,0:2')->px;

ok(!tapprox(ones(byte,9,3),$im2));

# here we encounter the problem
$im2 = $im1->clump(2)->nnslice(':,-1:0')->px;
ok(!tapprox(ones(byte,9,3),$im2));

$a = xvals( zeroes 10,10) + 0.1*yvals(zeroes 10,10);
ok(tapprox($a->nnslice('X',[6,7]),
	   pdl([
		[0.6, 1.6, 2.6, 3.6, 4.6, 5.6, 6.6, 7.6, 8.6, 9.6],
		[0.7, 1.7, 2.7, 3.7, 4.7, 5.7, 6.7, 7.7, 8.7, 9.7]
	       ])));

$lut = pdl [[1,0],[0,1]];
$im = pdl [1];
$in = $lut->xchg(0,1)->index($im->dummy(0));

is("$in", "
[
 [0 1]
]
");

$in .= pdl 1;

is("$in", "
[
 [1 1]
]
");
ok(tapprox($lut,pdl([[1,0],[1,1]])));

# can we catch indices which are too negative?
$a = PDL->sequence(10);
$b = $a->nnslice('0:-10');
is("$b", "[0]", "slice 0:-n picks first element");

$b = $a->nnslice('0:-14');
eval '"$b"';
like($@, qr/(out of bounds)|(Negative slice cannot start or end above limit)/);

# Test of dice and dice_axis
$a = sequence(10,4);
is($a->dice([1,2],[0,3])->sum, 66, "dice");
is($a->dice([0,1],'X')->sum, 124, "dice 'X'");

# Test of Reorder:
$a = sequence(5,3,2);
my @newDimOrder = (2,1,0);
$b = $a->reorder(@newDimOrder);

# since doing floating-point arithmetic here, should probably
# use a better test than "eq" here
#
is($b->average->average->sum , 72.5, "Test of reorder");

$a = zeroes(3,4);
$b = $a->dummy(-1,2);
is(join(',',$b->dims), '3,4,2');

$a = pdl(2);

$b = $a->nnslice('');
ok(tapprox($a, $b), "Empty slice");

$a = pdl [1,1,1,3,3,4,4,1,1,2];
$b = null;
$c = null;
rle($a,$b,$c);
ok(tapprox($a, rld($b,$c)));

$b = $a->nnslice(0.5);
ok(tapprox($b, 1), "mslice 1");

$b = $a->nnslice([0.5,2.11]);
is("$b", "[1 1 1]", "mslice 2");

$a = zeroes(3,3);
$b = $a->splitdim(3,3);
eval '$b->make_physdims';
like($@, qr/^Splitdim: nthdim/, "make_physdim: Splitdim");

$a = sequence 5,5;
$b = $a->diagonal(0,1);
is("$b", "[0 6 12 18 24]", "diagonal");

$a = sequence 10;
eval '$b = $a->lags(1,1,1)->make_physdims';
like($@, qr/lags: dim out of range/, "make_physdim: out of range");

eval '$b = $a->lags(0,-1,1)->make_physdims';
like($@, qr/lags: step must be positive/, "make_physdim: negative step");

eval '$b = $a->lags(0,1,11)->make_physdims';
like($@, qr/too large/, "make_pyhsdim: too large");

##############################
# Tests of permissive slicing and dummying

$a = xvals(5,5)+10*yvals(5,5);

eval '$b = $a->nnslice("1,2,(0)")->make_physical';
ok(!$@);
is($b->ndims, 2, "nnslice->make_physical: ndims");
is(pdl($b->dims)->sumover, 2, "nnslice->make_physical: dims");

eval '$c = $a->nnslice("1,2,(1)")->make_physical';
like($@, qr/(out of bounds)|(too many dims)/i, "nnslice->make_physical: too many dims");

# Hmmm, think these could be split up but not sure exactly what is being
# tested so leave as is (ish)
#
eval '$d = $a->nnslice("0:1,2:3,0")->make_physical';
ok(!$@);
ok(eval '$d->ndims == 3 && ((pdl($d->dims) == pdl(2,2,1))->sumover == 3)' && !$@);

eval '$d = $a->nnslice("0:1,2:3,0")->xchg(0,2)';
ok(!$@, "nnslice->xchg");

ok(eval '$d->ndims == 3 && ((pdl($d->dims) == pdl(1,2,2))->sumover == 3)' && !$@);

eval '$e = $a->dummy(6,2)';
ok(!$@, "dummy");

ok(eval '$e->ndims == 7 && ((pdl($e->dims) == pdl(5,5,1,1,1,1,2))->sumover==7)' && !$@);
