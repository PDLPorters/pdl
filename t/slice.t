# Test ->slice(). This is not yet good enough: we need
# nasty test cases
#
# Okay -- here're a couple (CED 3-apr-2002).  
#	 Added permissive-slicing tests, 42-50...

use PDL::LiteF;
# kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

# PDL::Core::set_debugging(1);

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub tapprox {
	my($a,$b) = @_;
	$c = abs($a-$b);
	$d = max($c);
	$d < 0.01;
}

print "1..63\n";

if(1) {


{

$a = (1+(xvals zeroes 4,5) + 10*(yvals zeroes 4,5));

print "FOO\n";

print $a;

print "BAR\n";

ok(1,$a->at(2,2) == 23);

$b = $a->slice('1:3:2,2:4:2');

# print $a; print $b;

ok(2,$b->at(0,0) == 22);
ok(3,$b->at(1,0) == 24);
ok(4,$b->at(0,1) == 42);
ok(5,$b->at(1,1) == 44);

$b .= 0.5 * double ones(2,2);

 print $a;

ok(6,$a->at(2,2) == 23);   # Check that nothing happened to other elems
ok(7,$a->at(1,2) == 0.5);

$a = pdl (1,2);
$b = pdl [[1,2],[1,2],[1,2]];
$c = $a->slice(',*3');

print $a,$b,$c;

# $c = $a->dummy(1,3);
sumover($c->clump(-1),($sum=null));
# check dimensions, sum of elements and correct order of els (using tapprox)
ok(8,tapprox($b,$c));
ok(9,$sum->at == 9);
ok(10,(join ',',$c->dims) eq "2,3");

$b = pdl [[1,1,1],[2,2,2]];
$c = $a->slice('*3,');
sumover($c->clump(-1),($sum=null));
# check dimensions, sum of elements and correct order of els (using tapprox)
ok(11,tapprox($b,$c));
ok(12,$sum->at == 9);
ok(13,(join ',',$c->dims) eq "3,2");

ok(14,1);  # test 14 moved to it's own script; too lazy to renumber
 }

# test stringify
$a = zeroes(3,3);
$line = $a->slice(':,(0)');

$a++;
# $line += 0; # that's how to force an update before interpolation
$linepr = "$line";


ok(15,$linepr eq '[1 1 1]');

# Test whether error is properly returned:

$b = zeroes(5,3,3);
$c = $b->slice(":,:,1");

ok(16,(join ',',$c->dims) eq "5,3,1");
eval {my $d = $c->slice(":,:,2"); print $d;};

print "ERROR WAS: '$@'\n";
ok(17,$@ =~ /Slice cannot start or end/i);



$a = zeroes 3,3;
print $a;


$b = $a->slice("1,1:2");
# print $b;
kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.
$b .= 1;

print $b;
print $a;

if(1) {

$a = xvals zeroes 20,20;
print $a;
kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

$b = $a->slice("1:18:2,:");
$c = $b->slice(":,1:18:2");
$d = $c->slice("3:5,:");
$e = $d->slice(":,(0)");
$f = $d->slice(":,(1)");

kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.
print "TOPRINT\n";

# print $b;
print $e,$f;
print $d,$c,$b,$a;

ok(18,"$e" eq "[7 9 11]");
ok(19,"$f" eq "[7 9 11]");

}
}

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

ok(20,(max(abs($e))) == 0);

print "OUTOUTOUT!\n";

use PDL::Dbg;


$im = byte [[0,1,255],[0,0,0],[1,1,1]];
($im1 = null) .= $im->dummy(0,3);
# print("1..2\n");
print $im1;
print ($im2 = $im1->clump(2)->slice(':,0:2')->px);

ok(21,!tapprox(ones(byte,9,3),$im2));

# here we encounter the problem
print ($im2 = $im1->clump(2)->slice(':,-1:0')->px);
ok(22,!tapprox(ones(byte,9,3),$im2));

$a = xvals( zeroes 10,10) + 0.1*yvals(zeroes 10,10);
ok(23, tapprox($a->mslice('X',[6,7]),pdl([
  [0.6, 1.6, 2.6, 3.6, 4.6, 5.6, 6.6, 7.6, 8.6, 9.6],
  [0.7, 1.7, 2.7, 3.7, 4.7, 5.7, 6.7, 7.7, 8.7, 9.7]
])));

$lut = pdl [[1,0],[0,1]];
$im = pdl [1];
$in = $lut->xchg(0,1)->index($im->dummy(0));

ok(24, tapprox($in,pdl([0,1])));

$in .= pdl 1;

ok(25, tapprox($in,pdl([1,1])));

ok(26, tapprox($lut,pdl([[1,0],[1,1]])));

# can we catch indices which are to negative
$a = PDL->sequence(10);
$b = $a->slice('0:-10');

ok(27, tapprox($b,pdl([0])));
$b = $a->slice('0:-14');
eval 'print $b';
ok(28, $@ =~ /Negative slice cannot start or end above limit/);

# Test of dice and dice_axis
$a = sequence(10,4);
ok(29, tapprox( $a->dice([1,2],[0,3])->sum , pdl(66) ) );
ok(30, tapprox $a->dice([0,1],'X')->sum, pdl(124));

# Test of Reorder:
$a = sequence(5,3,2);
@newDimOrder = (2,1,0);
$b = $a->reorder(@newDimOrder);

ok(31, tapprox($b->average->average->sum , pdl(72.5) ) );

$a = zeroes(3,4);
$b = $a->dummy(-1,2);
ok(32,join(',',$b->dims) eq '3,4,2');

$a = pdl(2);
print "a\n";
$b = $a->slice('');
ok(33,tapprox $a, $b);

$a = pdl[1,1,1,3,3,4,4,1,1,2];
$b = null;
$c = null;
rle($a,$b,$c);
ok(34,tapprox $a, rld($b,$c));

$b = $a->mslice(0.5);
ok(35, tapprox $b, 1);

$b = $a->mslice([0.5,2.11]);
ok(36, tapprox $b, ones(3));

$a = zeroes(3,3);
$b = $a->splitdim(3,3);
eval '$b->make_physdims';
ok(37,$@ =~ /^Splitdim: nthdim/);

$a = sequence 5,5;
$b = $a->diagonal(0,1);
ok(38, tapprox $b, sequence(5)*6);

$a = sequence 10;
eval '$b = $a->lags(1,1,1)->make_physdims';
ok(39, $@ =~ /lags: dim out of range/);

eval '$b = $a->lags(0,-1,1)->make_physdims';
ok(40, $@ =~ /lags: step must be positive/);

eval '$b = $a->lags(0,1,11)->make_physdims';
ok(41, $@ =~ /too large/);

##############################
# Tests of permissive slicing and dummying

$a = xvals(5,5)+10*yvals(5,5);

eval '$b = $a->slice("1,2,(0)")->make_physical';
ok(42, !$@);
ok(43, $b->ndims == 2 && pdl($b->dims)->sumover == 2);

eval '$c = $a->slice("1,2,(1)")->make_physical';
ok(44, $@=~ /too many dims/i);

eval '$d = $a->slice("0:1,2:3,0")->make_physical';
ok(45, !$@);
ok(46,eval '$d->ndims == 3 && ((pdl($d->dims) == pdl(2,2,1))->sumover == 3)' && !$@);

eval '$d = $a->slice("0:1,2:3,0")->xchg(0,2)';
ok(47, !$@);
ok(48,eval '$d->ndims == 3 && ((pdl($d->dims) == pdl(1,2,2))->sumover == 3)' && !$@);

eval '$e = $a->dummy(6,2)';
ok(49, !$@);
ok(50,eval '$e->ndims == 6 && ((pdl($e->dims) == pdl(5,5,1,1,1,2))->sumover==6)' && !$@);


##############################
# Tests of indexND (Nowadays this is just another call to range)

# Basic indexND operation
$source = 10*xvals(10,10) + yvals(10,10);
$index  = pdl([[2,3],[4,5]],[[6,7],[8,9]]);
eval '$a = $source->indexND( $index )';
ok(51,!$@);
ok(52,eval 'zcheck($a != pdl([23,45],[67,89]))');


# Threaded indexND operation
$source = 100*xvals(10,10,2)+10*yvals(10,10,2)+zvals(10,10,2);
$index  = pdl([[2,3],[4,5]],[[6,7],[8,9]]);
eval '$a = $source->indexND($index)';
ok(53,!$@);
ok(54,eval 'zcheck($a != pdl([[230,450],[670,890]],[[231,451],[671,891]]))');


##############################
# Tests of range operator

# Basic range operation
$source = 10*xvals(10,10) + yvals(10,10);
$index = pdl([[2,3],[4,5]],[[6,7],[8,9]]);

eval '$dest = $source->range($index);';
ok(55,!$@);
ok(56,eval 'zcheck($dest != pdl([23,45],[67,89]));');

# Make a 3x3 range at each index
eval '$dest = $source->range($index,3);';
ok(57,!$@);

# Check that the range has the correct size
ok(58,$dest->ndims == 4 && zcheck(pdl($dest->dims) != pdl(2,2,3,3)));

#### Check boundary conditions
eval '$z = $dest->copy;'; # Should throw range-out-of-bounds error
ok(59,$@);

## Truncation
eval '$z = $source->range($index,3,"t")->copy;';
ok(60,!$@);  # Should NOT throw range-out-of-bounds error.
ok(61, zcheck($z->slice("(1),(1)") != pdl([[89,99,0],[0,0,0],[0,0,0]])));

## Truncation on one axis, periodic on another; string syntax
eval '$z = $source->range($index,3,"tp")';
ok(62, zcheck($z->slice("(1),(1)") != pdl([[89,99,0],[80,90,0],[81,91,0]])));

## Periodic on first axis, extension on another; list syntax
eval '$z = $source->range($index,3,["e","p"]);';
ok(63, zcheck($z->slice("(1),(1)") != pdl([[89,99,99],[80,90,90],[81,91,91]])));

