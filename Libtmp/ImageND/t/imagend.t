use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use PDL::ImageND;
use PDL::NiceSlice;

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
$pa(4,:) .= 1;
$pa(5,:) .= 1;
$pa(1,2) .= 1;
$pa(0,4) .= 1;
$pa(2,0) .= 1;
my $pb = pdl( [-1,0],[0,1] );

{
	my $ans_e = pdl(
		     [ 0,  0,  1, -1,  0,  0],
		     [-1,  0,  0, -1,  0,  0],
		     [ 0,  1,  0, -1,  0,  0],
		     [ 0,  0,  0, -1,  0,  0],
		     [ 1,  0,  0, -1,  0,  0],
		     [ 0,  0,  0, -1,  0,  0]
		);
	my $pc = convolveND($pa,$pb,{m=>'d',b=>'e'});
	ok( all PDL::approx($pc,$ans_e, $eps) ) or diag $pc;
	$pc = convolveND($pa,$pb,{m=>'f',b=>'e'});
	ok( all PDL::approx($pc,$ans_e, $eps) ) or diag $pc;
}

{
	my $ans_p = pdl(
		     [ 0,  0,  1, -1,  0,  1],
		     [-1,  0,  0, -1,  0,  1],
		     [ 0,  1,  0, -1,  0,  1],
		     [ 0,  0,  0, -1,  0,  0],
		     [ 1,  0,  0, -1,  0,  1],
		     [ 0, -1,  0, -1,  0,  1]
		);
	my $pc = convolveND($pa,$pb,{m=>'d',b=>'p'});
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

{
my $got = circ_mean_p(sequence(8,8));
my $expected = pdl('[36 36 36 36 23.14285 14.4]');
ok all approx($got, $expected, 1e-3) or diag "got: $got\nexp: $expected";
}

{
my $got = circ_mean(sequence(2,2));
my $expected = pdl('[[1 1][1 3]]');
ok all approx($got, $expected, 1e-3) or diag "got: $got\nexp: $expected";
}

{
# cut down from demo 3d
my $size = 5;
my $x = xvals($size+1,$size+1) / $size;
my $y = yvals($size+1,$size+1) / $size;
my $z = 0.5 + 0.5 * (sin($x*6.3) * sin($y*6.3)) ** 3;
my $cvals = pdl q[0.203 0.276];
my $points = cat($x,$y,$z)->mv(-1,0);
my ($segs, $cnt) = contour_segments($cvals, $z, $points);
$segs = $segs->slice(',0:'.$cnt->max);
ok all(approx $cnt, pdl(15,15), 2), 'contour_segments' or diag $segs, $cnt;

$z = pdl q[
  0 0 0 0 0;
  0 0 1 0 0;
  0 1 0 1 0;
  0 1 1 1 0;
  0 0 0 0 0
];
(my $got, $cnt) = contour_segments(0.5, $z, $z->ndcoords);
$got = $got->slice(',0:'.$cnt->max)->uniqvec;
my $exp = pdl q[
 [0.5   2] [0.5   3] [  1 1.5] [  1 3.5]
 [1.5   1] [1.5   2] [  2 0.5] [  2 1.5]
 [  2 2.5] [  2 3.5] [2.5   1] [2.5   2]
 [  3 1.5] [  3 3.5] [3.5   2] [3.5   3]
];
ok all(approx $got, $exp, 0.1), 'contour_segments' or diag $got, $exp;
}

for (
  [6, q[0 1; 2 3; 4 5], '[1 3 5]', '[4 5 2 3 0 1]', 1],
  [6, q[0 1; 1 2; 2 3; 3 1; 3 2; 4 5], '[1 6 8 -1 -1 -1]', '[4 5 0 1 2 3 2 3 1 -1 -1 -1]', 1],
  [9, q[0 1; 1 2; 2 3; 3 1; 3 2; 4 5; 6 7; 7 8; 8 6], '[1 6 8 12 -1 -1 -1 -1 -1]', '[4 5 0 1 2 3 2 3 1 6 7 8 6 -1 -1 -1 -1 -1]', 1],
  [6, q[0 1; 1 2; 2 3; 3 1; 3 2; 4 5], '[4 6 8 -1 -1 -1]', '[0 1 2 3 1 2 3 4 5 -1 -1 -1]', 0],
  [6, q[0 1; 2 1; 2 3; 3 1; 3 2; 4 5], '[4 6 8 -1 -1 -1]', '[0 1 2 3 1 2 3 4 5 -1 -1 -1]', 0],
) {
  my ($d, $e, $pindsexp, $pexp, $directed) = @$_;
  my ($pinds, $p) = path_join(pdl($e), $d, $directed);
  is "$p", $pexp;
  is "$pinds", $pindsexp;
}

{
  my ($pi, $p) = map pdl($_), '[4 6 8 -1 -1 -1]', '[0 1 2 3 1 2 3 4 5 -1 -1 -1]';
  my @segs = path_segs($pi, $p);
  $_ = "$_" for @segs;
  is_deeply \@segs, ['[0 1 2 3 1]', '[2 3]', '[4 5]'];
}

done_testing;
