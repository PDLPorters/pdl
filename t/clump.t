# Test ->clump(). This is not yet good enough: we need
# nasty test cases

use Test::More tests => 3;
use PDL::LiteF;

use strict;
use warnings;

$|=1;

#  PDL::Core::set_debugging(1);
kill 'INT',$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

#$a = zeroes(4,4) * zeroes(4,4);
# $a = zeroes(4,4) ;

#print $a;
#
#print $a->at(3,3);
#
#exit 4;

my $eps = 0.01; # tolerance for tests

if(0) {
	# TODO dead code
	my $a0 = zeroes(3,3);
	note $a0;
	my $b0 = 10 * $a0;
	note $b0;
}

{
	# TODO no test here
	my $pa0 = zeroes(3,3);
	#my $pa = $pa0->PDL::Core::new_or_inplace($a0);
	my $pa = $pa0->copy;
	my $pb = $pa->xchg(0,1);
	note $pa;

	# PDL::Primitive::axisvalues($pb);
	# note $pa;
}

{
	# TODO no test here
	my $pa0 = xvals(zeroes(3,3));
	my $pa1 = yvals(zeroes(3,3));
	my $pa2 = 10*$pa1;
	my $pa3 = $pa0 + $pa1;

	for my $p ( $pa0, $pa1, $pa2, $pa3 ) {
		note $p;
	}
}


{
	my $pa = xvals(zeroes(3,3)) + 10*yvals(zeroes(3,3));
	note $pa;
	my $pb = $pa->clump(-1);
	# $b->make_physical();
	# $a->jdump();
	# $b->jdump();

	note $pb;

	ok(all PDL::approx($pb,pdl([0,1,2,10,11,12,20,21,22]), $eps));

	# note $b;

	my $pc = $pa->slice('0:2:2,:');

	my $pd = $pc->clump(-1);

	my $pe = $pd->slice("2:4");

	my $pf = ""; # Warning eater
	$pf= $pe->copy();

	kill 'INT',$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

	# ok(2,$@ =~ /^clump: Increments do not match/);
	# Clump supports this now.

	ok(all PDL::approx($pd,pdl([0,2,10,12,20,22]), $eps));

	ok(all PDL::approx($pe,pdl([10,12,20]), $eps));
}
