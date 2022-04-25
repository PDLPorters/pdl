use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use PDL::Dbg;

# PDL::Core::set_debugging(1);

# Useful for debugging. Removed by DJB whilst cleaning up the
# tests
#
#sub kill_if_debug () {
#    kill INT,$$  if $ENV{UNDER_DEBUGGER};
#}

sub tapprox ($$) {
    my $x = shift;
    my $y = shift;
    my $maxdiff = abs($x-$y)->max;
    return $maxdiff < 0.01;
}

my ($x, $y, $c, $d, $e, $f);

$x = (1+(xvals zeroes 4,5) + 10*(yvals zeroes 4,5));

is($x->at(2,2), 23, "x location (2,2) is 23");

$y = $x->slice('1:3:2,2:4:2');

is($y->at(0,0), 22, "(1,2)->(0,0)");
is($y->at(1,0), 24, "(3,2)->(1,0)");
is($y->at(0,1), 42, "(1,4)->(0,1)");
is($y->at(1,1), 44, "(3,4)->(1,1)");

$y .= 0.5 * ones(2,2);

is($y->at(1,0), 0.5);
is($y->at(0,1), 0.5);

is($x->at(1,2), 0.5);

# Check that nothing happened to other elems
is($x->at(2,2), 23);

$x = pdl (1,2);
$y = pdl [[1,2],[1,2],[1,2]];
$c = $x->slice(',*3');

# check dimensions, sum of elements and correct order of els (using tapprox)

my $sum;
# $c = $x->dummy(1,3);
sumover($c->clump(-1),($sum=null));
ok(tapprox($y,$c));
is($sum->at, 9);

is(join(',',$c->dims), "2,3");

$y = pdl [[1,1,1],[2,2,2]];
$c = $x->slice('*3,');
sumover($c->clump(-1),($sum=null));

ok(tapprox($y,$c));
is($sum->at, 9, 'sum of dummy=3 slice gives right value');
is(join(',',$c->dims), "3,2", 'right dims with dummy slice');

# test stringify
$x = zeroes(3,3);
my $line = $x->slice(':,(0)');

$x++;
# $line += 0; # that's how to force an update before interpolation
my $linepr = "$line";

is($linepr, '[1 1 1]', 'right value after collapsing slice (0)');

# Test whether error is properly returned:

$y = zeroes(5,3,3);
$c = $y->slice(":,:,1");

is(join(',',$c->dims), "5,3,1", 'single-coord slice dims right');

eval { my $d = $c->slice(":,:,2"); $d->string };

like($@, qr/out of bounds/, 'check slice bounds error handling') or diag "ERROR WAS: '$@'\n" if $@;

$x = zeroes 3,3;

$y = $x->slice("1,1:2");

$y .= 1;

$x = xvals zeroes 20,20;

$y = $x->slice("1:18:2,:");
$c = $y->slice(":,1:18:2");
$d = $c->slice("3:5,:");
$e = $d->slice(":,(0)");
$f = $d->slice(":,(1)");

$y->string;
$c->string; 
$d->string;
$e->string;
$f->string;

is("$e", "[7 9 11]");
is("$f", "[7 9 11]");

# Make sure that vaffining is properly working:

$x = zeroes 5,6,2;

$y = (xvals $x) + 0.1 * (yvals $x) + 0.01 * (zvals $x);

$y = $y->copy;

$c = $y->slice("2:3");

$d = $c->copy;

# $c->dump;
# $d->dump;

$e = $c-$d;

is(max(abs($e)), 0);

my ($im, $im1, $im2, $lut, $in);

$im = byte [[0,1,255],[0,0,0],[1,1,1]];
($im1 = null) .= $im->dummy(0,3);
$im2 = $im1->clump(2)->slice(':,0:2')->px;

ok(!tapprox(ones(byte,9,3),$im2));

# here we encounter the problem
$im2 = $im1->clump(2)->slice(':,-1:0')->px;
ok(!tapprox(ones(byte,9,3),$im2));

$x = xvals( zeroes 10,10) + 0.1*yvals(zeroes 10,10);
ok(tapprox($x->mslice('X',[6,7]),
	   pdl([
		[0.6, 1.6, 2.6, 3.6, 4.6, 5.6, 6.6, 7.6, 8.6, 9.6],
		[0.7, 1.7, 2.7, 3.7, 4.7, 5.7, 6.7, 7.7, 8.7, 9.7]
	       ])));

$lut = pdl [[1,0],[0,1]];
$im = pdl [1];
$in = $lut->transpose->index($im->dummy(0));

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
$x = PDL->sequence(10);
$y = $x->slice('0:-10');
is("$y", "[0]", "slice 0:-n picks first element");

$y = $x->slice('0:-14');
eval { $y->string };
like($@, qr/slice ends out of bounds/);

# Test of dice and dice_axis
$x = sequence(10,4);
is($x->dice([1,2],[0,3])->sum, 66, "dice");
is($x->dice([0,1],'X')->sum, 124, "dice 'X'");

# Test of dice clump compatability
my $xxx = PDL->new([[[0,0]],[[1,1]],[[2,2]]]);
is_deeply($xxx->where($xxx == 0)->unpdl,[0,0],"dice clump base zero");
my $dice = $xxx->dice("X","X",[1,0]);
is_deeply($dice->clump(-1)->unpdl,[1,1,0,0],"dice clump correct");
is_deeply($dice->where($dice == 0)->unpdl,[0,0],"dice clump where zero");

# Test of Reorder:
$x = sequence(5,3,2);
my @newDimOrder = (2,1,0);
$y = $x->reorder(@newDimOrder);

# since doing floating-point arithmetic here, should probably
# use a better test than "eq" here
#
my $got = [$y->dims];
is_deeply($got, [2,3,5], "Test of reorder") or diag explain $got;

$x = zeroes(3,4);
$y = $x->dummy(-1,2);
is(join(',',$y->dims), '3,4,2');

$x = pdl(2);
$y = $x->slice('');
ok(tapprox($x, $y), "Empty slice");

$x = pdl [1,1,1,3,3,4,4,1,1,2];
$y = null;
$c = null;
rle($x,$y,$c);
ok(tapprox($x, rld($y,$c)),"rle with null input");

undef $y; undef $c;
($y,$c) = rle($x);
ok(tapprox($x, rld($y,$c)),"rle with return vals");

my $x2d = $x->cat($x->rotate(1),$x->rotate(2),$x->rotate(3),$x->rotate(4));
rle($x2d,$y=null,$c=null);
ok(tapprox($x2d,rld($y,$c)),"rle 2d with null input");

undef $y; undef $c;
($y,$c) = rle($x2d);
ok(tapprox($x2d, rld($y,$c)),"rle 2d with return vals");

$y = $x->mslice(0.5);
ok(tapprox($y, 1), "mslice 1");

$y = $x->mslice([0.5,2.11]);
is("$y", "[1 1 1]", "mslice 2");

$x = zeroes(3,3);
$y = $x->splitdim(3,3);
eval { $y->make_physdims };
like($@, qr/splitdim:nthdim/, "make_physdim: Splitdim");
$y = $x->splitdim(-1,1);
is_deeply [$y->dims], [3,1,3], 'splitdims negative nthdim works' or diag explain [$y->dims];
$y = $x->splitdim(1,1);
is_deeply [$y->dims], [3,1,3], 'splitdims works' or diag explain [$y->dims];
$y = $x->splitdim(1,2);
eval { $y->make_physdims };
like($@, qr/non-divisible/, "splitdims error non-divisible");

$x = sequence 5,5;
$y = $x->diagonal(0,1);
is("$y", "[0 6 12 18 24]", "diagonal");

$x = sequence 10;
eval { $y = $x->lags(1,1,1)->make_physdims };
like($@, qr/lags:\s*dim out of range/, "make_physdim: out of range");

eval { $y = $x->lags(0,-1,1)->make_physdims };
like($@, qr/lags:\s*step must be positive/, "make_physdim: negative step");

eval { $y = $x->lags(0,1,11)->make_physdims };
like($@, qr/too large/, "make_physdim: too large");

##############################
# Tests of some edge cases
$x = sequence(10);
eval { $y = $x->slice("5") };
is $@, '', "simple slice works";
ok(($y->nelem==1 and $y==5), "simple slice works right");

eval { $y = $x->slice("5:") };
is $@, '', "empty second specifier works";
ok(($y->nelem == 5  and  all($y == pdl(5,6,7,8,9))), "empty second specifier works right");

eval { $y = $x->slice(":5") };
is $@, '', "empty first specifier works";
ok(($y->nelem == 6  and  all($y == pdl(0,1,2,3,4,5))), "empty first specifier works right");

##############################
# White space in slice specifier
eval {  $y = $x->slice(" 4:") };
is $@, '',"slice with whitespace worked - 1";
ok(($y->nelem==6 and all($y==pdl(4,5,6,7,8,9))),"slice with whitespace works right - 1");
eval { $y = $x->slice(" :4") };
is $@, '',"slice with whitespace worked - 2";
ok(($y->nelem==5 and all($y==pdl(0,1,2,3,4))),"slice with whitespace works right - 2");
eval { $y = $x->slice(" 3: 4 ") };
is $@, '',"slice with whitespace worked - 3";
ok(($y->nelem==2 and all($y==pdl(3,4))),"slice with whitespace works right - 3");

##############################
# Tests of permissive slicing and dummying

$x = xvals(5,5)+10*yvals(5,5);

eval { $y = $x->slice("1,2,(0)")->make_physical };
is $@, '';
is($y->ndims, 2, "slice->make_physical: ndims");
is(pdl($y->dims)->sumover, 2, "slice->make_physical: dims");

eval { $c = $x->slice("1,2,(1)")->make_physical };
like($@, qr/too many dims/i, "slice->make_physical: too many dims");

# Hmmm, think these could be split up but not sure exactly what is being
# tested so leave as is (ish)
#
eval { $d = $x->slice("0:1,2:3,0")->make_physical };
is $@, '';
is $d->ndims, 3;
is +(pdl($d->dims) == pdl(2,2,1))->sumover, 3;
is $d->ndims, 3;
is +(pdl($d->dims) == pdl(2,2,1))->sumover, 3;

eval { $d = $x->slice("0:1,2:3,0")->xchg(0,2) };
is $@, '', "slice->xchg";

is $d->ndims, 3;
is +(pdl($d->dims) == pdl(1,2,2))->sumover, 3;

eval { $e = $x->dummy(6,2) };
is $@, '', "dummy";

is $e->ndims, 7;
is +(pdl($e->dims) == pdl(5,5,1,1,1,1,2))->sumover, 7;

##############################
# Tests of indexND (Nowadays this is just another call to range)

my ($source, $index, $dest, $z);

# Basic indexND operation
$source = 10*xvals(10,10) + yvals(10,10);
$index  = pdl([[2,3],[4,5]],[[6,7],[8,9]]);
eval { $x = $source->indexND( $index ) };
is $@, '';
ok(eval { zcheck($x != pdl([23,45],[67,89])) }, "eval of zcheck 1");

# Broadcast indexND operation
$source = 100*xvals(10,10,2)+10*yvals(10,10,2)+zvals(10,10,2);
$index  = pdl([[2,3],[4,5]],[[6,7],[8,9]]);
eval { $x = $source->indexND($index) };
is $@, '';
ok(eval { zcheck($x != pdl([[230,450],[670,890]],[[231,451],[671,891]])) }, "eval of zcheck 2");


##############################
# Tests of range operator

# Basic range operation
$source = 10*xvals(10,10) + yvals(10,10);
$index = pdl([[2,3],[4,5]],[[6,7],[8,9]]);

eval { $dest = $source->range($index); };
is $@, '';
ok(eval { zcheck($dest != pdl([23,45],[67,89])); }, "eval of zcheck 3");

# Make a 3x3 range at each index
eval { $dest = $source->range($index,3); };
is $@, '';

# Check that the range has the correct size
is($dest->ndims, 4, "ndims after range");
ok(zcheck(pdl($dest->dims) != pdl(2,2,3,3)), "zcheck after range");

#### Check boundary conditions
eval { $z = $dest->copy; }; # Should throw range-out-of-bounds error
ok($@); # should check actual error message here

## Truncation
eval { $z = $source->range($index,3,"t")->copy; };
is $@, '';  # Should NOT throw range-out-of-bounds error.
ok(zcheck($z->slice("(1),(1)") != pdl([[89,99,0],[0,0,0],[0,0,0]])));

## Truncation on one axis, periodic on another; string syntax
eval { $z = $source->range($index,3,"tp") };
ok(zcheck($z->slice("(1),(1)") != pdl([[89,99,0],[80,90,0],[81,91,0]])));

## Periodic on first axis, extension on another; list syntax
eval { $z = $source->range($index,3,["e","p"]); };
ok(zcheck($z->slice("(1),(1)") != pdl([[89,99,99],[80,90,90],[81,91,91]])));

our $mt;
eval { $mt = which(pdl(0)) };
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

$x = pdl(5,5,5,5);
$z = $x->range($mt);
ok("$z" eq 'Empty[0]');

$z .= 2; # should *not* segfault!
ok all($x==5), 'empty range .= no mutate';   # should *not* change $x!

### Check slicing of a null PDL
$x = PDL->null;
eval { $y = $x->slice("") };
like $@, qr/is null/, 'null->slice exception';

for my $start (0, 4, -4, 20, -20) {
	for my $stop (0, 4, -4, 20, -20) {
		# Generate a simple data ndarray and a bad slice of that ndarray
		my $data = sequence(10);
		my $slice = $data->slice("$start:$stop");

		pass('Slice operation for properly formed slice does not croak');

		# Calculate the expected dimension size:
		my $expected_dim_size;
		my $real_start = $start;
		$real_start += 10 if $start < 0;
		my $real_stop = $stop;
		$real_stop += 10 if $stop < 0;
		$expected_dim_size = abs($real_stop - $real_start) + 1
			if 0 <= $real_stop and $real_stop < 10
				and 0 <= $real_start and $real_start < 10;

		my $expected_outcome_description
			= defined $expected_dim_size ? 'is fine' : 'croaks';

		my $dim1;
		# Should croak when we ask about the dimension:
		eval { $dim1 = $slice->dim(0) };
		is($dim1, $expected_dim_size, "Requesting dim(0) on slice($start:$stop) $expected_outcome_description");

		# Should *STILL* croak when we ask about the dimension:
		eval { $dim1 = $slice->dim(0) };
		is($dim1, $expected_dim_size, "Requesting dim(0) a second time on slice($start:$stop) $expected_outcome_description");

		# Calculate the expected value
		my $expected_value;
		$expected_value = $data->at($real_start) if defined $expected_dim_size;

		# Should croak when we ask about data
		my $value;
		eval { $value = $slice->at(0) };
		is($value, $expected_value, "Requesting first element on slice($start:$stop) $expected_outcome_description");
	}
}

{
# Test vaffine optimisation
my $x = zeroes(100,100);
my $y = $x->slice('10:90,10:90');
$y++;
ok( (not $y->allocated) ) ;
}

my $indices = pdl([]);
$got = eval { my $s = pdl([1,2])->slice(pdl(1)); $s->string; $s->nelem };
is $@, '', 'slice 2-elt ndarray with one-length ndarray';
is $got, 1, 'right dim from 2-elt with one index';
$got = eval { my $s = pdl([1,2])->slice($indices); $s->string; $s->nelem };
is $@, '', 'slice 2-elt ndarray with zero-length ndarray';
is $got, 0, 'zero dim from 2-elt';
$got = eval { my $s = pdl([1])->slice($indices); $s->string; $s->nelem };
is $@, '', 'slice 1-elt ndarray with zero-length ndarray';
is $got, 0, 'zero dim from 1-elt';

my $pa = sequence 10;
$c = PDL->pdl(7,6);
$got = $pa->slice([$c->slice(1),0,0]);
is "".$got, 6, 'slice did "at" automatically' or diag "got:$got";

my $cmp = pdl(2,4,6);
my $rg = pdl(2,7,2);
$got = $pa->slice([$rg->slice(0),$rg->slice(1),$rg->slice(2)]);
ok all($got == $cmp), 'slice did "at"' or diag "got:$got";

$pa = zeroes(7, 7); $pa->set(3, 4, 1);
$indices = $pa->which->dummy(0,$pa->getndims)->make_physical;
my $s = $indices->index(0);
$s %= 7;
is $indices.'', <<EOF, 'mutate indexed slice affects only right column';
\n[\n [ 3 31]\n]
EOF

done_testing;
