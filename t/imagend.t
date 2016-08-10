use PDL;
use PDL::ImageND;
use PDL::NiceSlice;

use Test::More tests => 8;
use strict;
use warnings;

kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

my $eps = 1e-15;

# Right answer


{
	my $ans = pdl(
	 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27],
	 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27],
	 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27]
	);
	my $pa = xvals zeroes 10,3;
	my $pb = pdl [1,2],[2,1];
	my $pc = convolve ($pa, $pb);
	ok(all PDL::approx( $pc, $ans, $eps ) );
}


my $pa = zeroes(6,6);
my $pb = pdl( [-1,0],[0,1] );

{
	# XXX Dead code
	my $ta;

	($ta = $pa(4,:)) .= 1;
	($ta = $pa(5,:)) .= 1;
	($ta = $pa(1,2)) .= 1;
	($ta = $pa(0,4)) .= 1;
	($ta = $pa(2,0)) .= 1;
}


{
	my $pc;
	my $ans_e = pdl(
		     [ 0,  0,  1, -1,  0,  0],
		     [-1,  0,  0, -1,  0,  0],
		     [ 0,  1,  0, -1,  0,  0],
		     [ 0,  0,  0, -1,  0,  0],
		     [ 1,  0,  0, -1,  0,  0],
		     [ 0,  0,  0, -1,  0,  0]
		);
	$pc = convolveND($pa,$pb,{m=>'d',b=>'e'});
	ok( all PDL::approx($pc,$ans_e, $eps) );

	$pc = convolveND($pa,$pb,{m=>'f',b=>'e'});
	ok( all PDL::approx($pc,$ans_e, $eps) );
}

{
	my $pc;
	my $ans_p = pdl(
		     [ 0,  0,  1, -1,  0,  1],
		     [-1,  0,  0, -1,  0,  1],
		     [ 0,  1,  0, -1,  0,  1],
		     [ 0,  0,  0, -1,  0,  0],
		     [ 1,  0,  0, -1,  0,  1],
		     [ 0, -1,  0, -1,  0,  1]
		);

	$pc = convolveND($pa,$pb,{m=>'d',b=>'p'});
	ok( all( PDL::approx($pc, $ans_p, $eps) ) );

	$pc = convolveND($pa,$pb,{m=>'f',b=>'p'});
	ok(all PDL::approx($pc, $ans_p, $eps) );
}


{
	my $pc;
	my $ans_t = pdl(
		     [ 0,  0,  1, -1,  0,  1],
		     [-1,  0,  0, -1,  0,  1],
		     [ 0,  1,  0, -1,  0,  1],
		     [ 0,  0,  0, -1,  0,  1],
		     [ 1,  0,  0, -1,  0,  1],
		     [ 0,  0,  0,  0,  1,  1]
		);
	$pc = convolveND($pa,$pb,{m=>'d',b=>'t'});
	ok(all PDL::approx($pc,$ans_t, $eps) );

	$pc = convolveND($pa,$pb,{m=>'f',b=>'t'});
	ok( all( PDL::approx($pc, $ans_t, $eps) ) );
}

{
    my $pa = sequence(6,6);
    my $ans = pdl([14,22,30],[62,70,78],[110,118,126]);
    ok( all( $ans==rebin($pa,3,3,{Norm=>1}) ) );

}

done_testing;
