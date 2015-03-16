# Test ->clump(). This is not yet good enough: we need
# nasty test cases

use Test::More tests => 3;
use PDL::LiteF;

$|=1;

#  PDL::Core::set_debugging(1);
kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub tapprox {
	my($a,$b) = @_;
	my $c = abs($a-$b);
	my $d = max($c);
	print "C AND D: $a,$b,$c,$d\n";
	$d < 0.01;
}

$a0 = zeroes(3,3);
$a = $a0->copy;
$b = $a->xchg(0,1);
note $a;

$a0 = xvals(zeroes(3,3));

note $a0;

$a1 = yvals(zeroes(3,3));

note $a1;

$a2 = 10*$a1;

note $a2;

$a3 = $a0 + $a1;

note $a3;

$a = xvals(zeroes(3,3)) + 10*yvals(zeroes(3,3));

 note $a;

$b = $a->clump(-1);

# $b->make_physical();

# $a->jdump();
# $b->jdump();

note $b;

ok tapprox($b,pdl [0,1,2,10,11,12,20,21,22]);

$c = $a->slice('0:2:2,:');

$d = $c->clump(-1);

$e = $d->slice("2:4");

$f = ""; # Warning eater

$f= $e->copy();;

kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.
# ok(2,$@ =~ /^clump: Increments do not match/);
# Clump supports this now.
ok tapprox($d,pdl [0,2,10,12,20,22]);

ok tapprox($e,pdl [10,12,20]);
