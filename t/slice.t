# -*-perl-*-
no warnings qw(misc);
#
# Test ->slice(). This is not yet good enough: we need
# nasty test cases
#
# Okay -- here're a couple (CED 3-apr-2002).
#	 Added permissive-slicing tests
#

use strict;
use Test::More;

plan tests => 73;

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

print "FOO\n";

print $a;

print "BAR\n";

is($a->at(2,2), 23);

$b = $a->slice('1:3:2,2:4:2');

# print $a; print $b;

is($b->at(0,0), 22);
is($b->at(1,0), 24);
is($b->at(0,1), 42);
is($b->at(1,1), 44);

#$b .= 0.5 * double ones(2,2);
$b .= 0.5 * ones(2,2);

is($b->at(1,0), 0.5);
is($b->at(0,1), 0.5);

print $a;

is($a->at(1,2), 0.5);

# Check that nothing happened to other elems
is($a->at(2,2), 23);

$a = pdl (1,2);
$b = pdl [[1,2],[1,2],[1,2]];
$c = $a->slice(',*3');

print $a,$b,$c;

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

eval { my $d = $c->slice(":,:,2"); print $d; };

like($@, qr/Slice cannot start or end/, 'check slice bounds error handling') or diag "ERROR WAS: '$@'\n" if $@;

$a = zeroes 3,3;
print $a;

$b = $a->slice("1,1:2");

# print $b;

$b .= 1;

print $b;
print $a;

$a = xvals zeroes 20,20;
print $a;

$b = $a->slice("1:18:2,:");
$c = $b->slice(":,1:18:2");
$d = $c->slice("3:5,:");
$e = $d->slice(":,(0)");
$f = $d->slice(":,(1)");

print "TOPRINT\n";

# print $b;
print $e,$f;
print $d,$c,$b,$a;

is("$e", "[7 9 11]");
is("$f", "[7 9 11]");

# Make sure that vaffining is properly working:

$a = zeroes 5,6,2;

$b = (xvals $a) + 0.1 * (yvals $a) + 0.01 * (zvals $a);

$b = $b->copy;

print $b;

$c = $b->slice("2:3");

$d = $c->copy;

# $c->dump;
# $d->dump;

$e = $c-$d;

print $e;

print $c;
print $d;

# $c->dump; $d->dump;

is(max(abs($e)), 0);

print "OUTOUTOUT!\n";

use PDL::Dbg;

my ($im, $im1, $im2, $lut, $in);

$im = byte [[0,1,255],[0,0,0],[1,1,1]];
($im1 = null) .= $im->dummy(0,3);
# print("1..2\n");
print $im1;
print ($im2 = $im1->clump(2)->slice(':,0:2')->px);

ok(!tapprox(ones(byte,9,3),$im2));

# here we encounter the problem
print ($im2 = $im1->clump(2)->slice(':,-1:0')->px);
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
eval 'print $b';
like($@, qr/Negative slice cannot start or end above limit/);

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
print "a\n";
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
ok("$mt" eq 'Empty');

our $dex = pdl(5,4,3);
$z = $dex->range($mt);
ok("$z" eq 'Empty');

$z = $mt->range($dex,undef,'e');
ok(all($z==0));

$z = $mt->range($mt);
ok("$z" eq 'Empty');

$a = pdl(5,5,5,5);
$z = $a->range($mt);
ok("$z" eq 'Empty');

$z .= 2;
ok(1);            # should *not* segfault!
ok(all($a==5));   # should *not* change $a!
