# Test ->clump(). This is not yet good enough: we need
# nasty test cases

use PDL::LiteF;

$|=1;

#  PDL::Core::set_debugging(1);
kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.


#$a = zeroes(4,4) * zeroes(4,4);
# $a = zeroes(4,4) ;

#print $a;
#
#print $a->at(3,3);
#
#exit 4;


sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub tapprox {
	my($a,$b) = @_;
	my $c = abs($a-$b);
	my $d = max($c);
	print "C AND D: $a,$b,$c,$d\n";
	$d < 0.01;
}

if(0) {
	$a0 = zeroes(3,3);
	print $a0;
	$b0 = 10 * $a0;
	print $b0;
}

$a0 = zeroes(3,3);
# $a = $a0->PDL::Core::new_or_inplace($a0);
$a = $a0->copy;
$b = $a->xchg(0,1);
print $a;
# PDL::Primitive::axisvalues($b);
# print $a;

print "1..3\n";

$a0 = xvals(zeroes(3,3));

print $a0;

$a1 = yvals(zeroes(3,3));

print $a1;

$a2 = 10*$a1;

print $a2;

$a3 = $a0 + $a1;

print $a3;

$a = xvals(zeroes(3,3)) + 10*yvals(zeroes(3,3));

 print $a;

$b = $a->clump(-1);

# $b->make_physical();

# $a->jdump();
# $b->jdump();

print $b;

ok(1,tapprox($b,pdl [0,1,2,10,11,12,20,21,22]));

# print $b;

$c = $a->slice('0:2:2,:');

$d = $c->clump(-1);

$e = $d->slice("2:4");

$f = ""; # Warning eater

$f= $e->copy();;

kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.
# ok(2,$@ =~ /^clump: Increments do not match/);
# Clump supports this now.
ok(2,tapprox($d,pdl [0,2,10,12,20,22]));

ok(3,tapprox($e,pdl [10,12,20]));
