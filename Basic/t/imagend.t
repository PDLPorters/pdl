use strict;
use warnings;
use Test::More;
use Test::PDL;
use PDL::LiteF;
use PDL::ImageND;
use PDL::NiceSlice;

my $eps = 1e-15;

{
	my $ans = pdl(
	 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27],
	 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27],
	 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27]
	);
	is_pdl convolve(xvals(10,3), pdl([1,2],[2,1])), $ans;
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
	is_pdl convolveND($pa,$pb,{m=>'d',b=>'e'}),$ans_e;
	is_pdl convolveND($pa,$pb,{m=>'f',b=>'e'}),$ans_e;
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
	is_pdl convolveND($pa,$pb,{m=>'d',b=>'p'}), $ans_p;
	is_pdl convolveND($pa,$pb,{m=>'f',b=>'p'}), $ans_p;
}

{
	my $ans_t = pdl(
		     [ 0,  0,  1, -1,  0,  1],
		     [-1,  0,  0, -1,  0,  1],
		     [ 0,  1,  0, -1,  0,  1],
		     [ 0,  0,  0, -1,  0,  1],
		     [ 1,  0,  0, -1,  0,  1],
		     [ 0,  0,  0,  0,  1,  1]
		);
	is_pdl convolveND($pa,$pb,{m=>'d',b=>'t'}), $ans_t;
	is_pdl convolveND($pa,$pb,{m=>'f',b=>'t'}), $ans_t;
}

{
    my $ans = pdl([14,22,30],[62,70,78],[110,118,126]);
    is_pdl rebin(sequence(6,6),3,3,{Norm=>1}), $ans;
}

is_pdl circ_mean_p(sequence(8,8)), pdl('[36 36 36 36 23.14285 14.4]');
is_pdl circ_mean(sequence(2,2)), pdl('[[1 1][1 3]]');

{
# cut down from demo 3d
my $size = 5;
my $x = xvals($size+1,$size+1) / $size;
my $y = yvals($size+1,$size+1) / $size;
my $z = 0.5 + 0.5 * (sin($x*6.3) * sin($y*6.3)) ** 3;
my $cvals = pdl q[0.203 0.276];
my $points = cat($x,$y,$z)->mv(-1,0);
my (undef, $cnt) = contour_segments($cvals, $z, $points);
is_pdl $cnt, indx(15,15), {atol=>2, test_name=>'contour_segments'};

$z = pdl q[
  0 0 0 0 0;
  0 0 1 0 0;
  0 1 0 1 0;
  0 1 1 1 0;
  0 0 0 0 0
];
(my $got, $cnt) = contour_segments(0.5, $z, my $coords = $z->ndcoords);
$got = $got->slice(',0:'.$cnt->max)->uniqvec;
my $exp = float q[
 [0.5   2] [0.5   3] [  1 1.5] [  1 3.5]
 [1.5   1] [1.5   2] [  2 0.5] [  2 1.5]
 [  2 2.5] [  2 3.5] [2.5   1] [2.5   2]
 [  3 1.5] [  3 3.5] [3.5   2] [3.5   3]
];
is_pdl $got, $exp, {atol=>0.1, test_name=>'contour_segments'};

my ($pi, $p) = contour_polylines(0.5, $z, $coords);
my $pi_max = $pi->max;
$p = $p->slice(','.($pi_max < 0 ? '1:0:1' : "0:$pi_max"))->uniqvec;
is_pdl $p, $exp, {atol=>0.1, test_name=>'contour_polylines'};
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
  is_deeply [map "$_", path_segs($pi, $p)], ['[0 1 2 3 1]', '[2 3]', '[4 5]'];
}

{
my ($x, $y, $z) = map float($_), 5..7;
my $c3 = combcoords($x,$y,$z);
is_pdl $c3, float(5,6,7);
$x++;
is_pdl $c3, float(6,6,7);
}

{
my $coords = float([0,-1,0], [-1,-1,-2], [3,5,2], [2,1,-3], [1,3,1], [1,1,2]);
my $from = indx([0,1,2,3,4,4,4,5,5,5]);
my $to =   indx([1,2,3,1,0,2,3,0,1,2]);
is_pdl repulse($coords,3,5000,-100,-5,-0.1,0.01),
  float('[-19.9785 -83.469 27.7307; -75.2499 -57.671 -78.2403;
    57.7644 92.1679 42.674; 47.2407 6.71818 -93.5201;
    -15.8246 75.1071 8.60458; 6.04795 -32.8531 92.7511]'), {atol=>1e-2};
is_pdl attract($coords,$from,$to,1,30,1),
  float('[172.197 779.117 199.115; 2054.32 2486.76 1963.34;
    -2004.3 -3652.66 -2542.66; -300.804 1010.14 1942.27;
    211.198 -700.071 -680.188; -132.611 76.7175 -881.878]'), {atol=>1e-2};
}

done_testing;
