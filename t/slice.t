# Test ->slice(). This is not yet good enough: we need
# nasty test cases,

use PDL::LiteF;
# kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

# PDL::Core::set_debugging(1);

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub approx {
	my($a,$b) = @_;
	$c = abs($a-$b);
	$d = max($c);
	$d < 0.01;
}

print "1..29\n";

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
# check dimensions, sum of elements and correct order of els (using approx)
ok(8,approx($b,$c));
ok(9,$sum->at == 9);
ok(10,(join ',',$c->dims) eq "2,3");

$b = pdl [[1,1,1],[2,2,2]];
$c = $a->slice('*3,');
sumover($c->clump(-1),($sum=null));
# check dimensions, sum of elements and correct order of els (using approx)
ok(11,approx($b,$c));
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

ok(21,!approx(ones(byte,9,3),$im2));

# here we encounter the problem
print ($im2 = $im1->clump(2)->slice(':,-1:0')->px);
ok(22,!approx(ones(byte,9,3),$im2));

$a = xvals( zeroes 10,10) + 0.1*yvals(zeroes 10,10);
ok(23, approx($a->mslice('X',[6,7]),pdl([
  [0.6, 1.6, 2.6, 3.6, 4.6, 5.6, 6.6, 7.6, 8.6, 9.6],
  [0.7, 1.7, 2.7, 3.7, 4.7, 5.7, 6.7, 7.7, 8.7, 9.7]
])));

$lut = pdl [[1,0],[0,1]];
$im = pdl [1];
$in = $lut->xchg(0,1)->index($im->dummy(0));

ok(24, approx($in,pdl([0,1])));

$in .= pdl 1;

ok(25, approx($in,pdl([1,1])));

ok(26, approx($lut,pdl([[1,0],[1,1]])));

# can we catch indices which are to negative
$a = PDL->sequence(10);
$b = $a->slice('0:-10');

ok(27, approx($b,pdl([0])));
$b = $a->slice('0:-14');
eval 'print $b';
ok(28, $@ =~ /Negative slice cannot start or end above limit/);

# Test of dice and dice_axis
$a = sequence(10,4);
ok(29, $a->dice([1,2],[0,3])->sum == 66 );
