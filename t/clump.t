use strict;
use warnings;

# Test ->clump(). This is not yet good enough: we need
# nasty test cases

use Test::More;
use PDL::LiteF;
use Test::PDL;

#  PDL::Core::set_debugging(1);
kill 'INT',$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub abstol { {atol=>0.01, test_name=>$_[0]} } # tolerance for tests

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
	my $pb = $pa->transpose;
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
	# $pb->make_physical();
	# $pa->jdump();
	# $pb->jdump();

	note $pb;

	is_pdl $pb, pdl([0,1,2,10,11,12,20,21,22]), abstol 'clump(-1) entire ndarray';

	my $pc = $pa->slice('0:2:2,:');

	my $pd = $pc->clump(-1);

	my $pe = $pd->slice("2:4");

	my $pf = ""; # Warning eater
	$pf= $pe->copy();

	kill 'INT',$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

	# ok(2,$@ =~ /^clump: Increments do not match/);
	# Clump supports this now.

	is_pdl $pd,pdl([0,2,10,12,20,22]), abstol 'clump(-1) slice with skip and whole dim';

	is_pdl $pe,pdl([10,12,20]), abstol 'clump(-1) slice';

        # SF bug #406 clump(-N) failure
        ##-- test data
        my $a1 = sequence(2,13);
        my $b1 = sequence(3,2,13);
        
        ##-- bash to max 2 dimensions
        my $a2 = $a1->clump(-2);   ##-- no-op
        my $b2 = $b1->clump(-2);   ##-- merge 1st 2 dims
        
        ok($a1->ndims == 2, "no-op clump(-2)");
        ok($b2->ndims == 2, "general clump(-2)");
}

done_testing;
