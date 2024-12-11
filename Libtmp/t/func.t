use strict;
use warnings;
use Test::More;
use Test::PDL;
use PDL::LiteF;
use PDL::Func qw(pchip spline);

my $x = float( 1, 2, 3, 4, 5, 6, 8, 10 );
my $y = ($x * 3) * ($x - 2);
my $xi = $x - 0.5;
my $obj = PDL::Func->init( x => $x, y => $y );
is( $obj->scheme() , 'Linear', 'default scheme is linear' );  # 1
is_pdl $obj->interpolate( $xi ), pdl('-4.5 -1.5 4.5 16.5 34.5 58.5 126 216'), {atol=>1e-5, test_name=>'linear interpolate'};
is $obj->status, -1, 'non serious error from linear interpolate: extrapolation used';
is_pdl $obj->get( 'err' ), long('1 0 0 0 0 0 0 0'), 'same error as direct';

eval { $obj->gradient( $xi ); };
like $@ , qr/can not call gradient/, 'calling unavailable method';

if (!eval { require PDL::Slatec; PDL::Slatec->import; 1 }) {
  done_testing;
  exit;
}

$x = sequence(float,10);
$y = $x*$x + 0.5;
$obj->set( Interpolate => 'Hermite', x => $x, y => $y );
is( $obj->scheme() , 'Hermite' , 'scheme is Hermite'); 
is( $obj->get('bc'), 'simple' , 'boundary condition is simple'); 
is( $obj->status, 1 , 'no errors');

$xi = sequence(float,5) + 2.3;
is_pdl $obj->interpolate( $xi ), $xi*$xi + 0.5, {atol=>0.04, test_name=>'interpolate'};
is( $obj->status, 1, 'status==1 after interpolate');

is_pdl scalar $obj->gradient( $xi ), 2*$xi, {atol=>0.04, test_name=>'gradient'};
is( $obj->status, 1, 'status==1 after gradient');

# see how they cope with broadcasting
$y = cat( $x*$x+43.3, $x*$x*$x-23 );
$obj->set( x => $x, y => $y );
is( $obj->status , 1, 'broadcasting: status==1 after set');
my $ans = cat( $xi*$xi+43.3, $xi*$xi*$xi-23 );
is_pdl $obj->interpolate( $xi ), $ans, {atol=>6, test_name=>'broadcasting'};
is( $obj->status, 1 ,'broadcasting: status==1 after interpolate');

# non-simple boundary conditions
$obj->set( bc => {} );
is_pdl $obj->interpolate( $xi ), $ans, {atol=>6, test_name=>'broadcasting non-simple'};

is_pdl pchip( $x, $y, $xi ), $ans, {atol=>6, test_name=>'pchip'};
is_pdl spline( $x, $y, $xi ), $ans, {atol=>6, test_name=>'spline'};

$x = sequence(10);
$y = pdl('43.3 44.3 47.3 52.3 59.3 68.3 79.3 92.3 107.3 124.3; -23 -22 -15 4 41 102 193 320 489 706');
my ($g) = chsp([0,0], [0,0], $x, $y);
is_pdl $g, pdl('0 2 4 6 8 10 12 14 16 18; 0 3 12 27 48 75 108 147 192 243'), 'chsp';
($g) = chic([0,0], [0,0], 0, $x, $y);
is_pdl $g, pdl('0 1.5 3.75 5.8333333 7.875 9.9 11.916667 13.928571 15.9375 18; 0 1.75 10.230769 25.107143 46.061224 73.039474 106.02752 145.02027 190.01554 241'), 'chic';
($g) = chim($x, $y);
is_pdl $g, pdl('0 1.5 3.75 5.8333333 7.875 9.9 11.916667 13.928571 15.9375 18; 0 1.75 10.230769 25.107143 46.061224 73.039474 106.02752 145.02027 190.01554 241'), 'chim';
my ($yi) = chfe($x, $y, $g, [1,1], $xi);
is_pdl $yi, my $yi_exp = pdl('48.56375 54.173375 61.777925 71.38055 82.98225; -10.973827 12.780893 56.345513 125.71307 226.88177'), 'chfe';
($yi) = chfd($x, $y, $g, [0,0], $xi);
is_pdl $yi, $yi_exp, 'chfd';
my ($integral) = chia($x, $y, $g, [0,0], 3, 4);
is_pdl $integral, pdl(55.6298611111111,20.7538265306122), 'chia';
($integral) = chid($x, $y, $g, [0,0], 3, 4);
is_pdl $integral, pdl(55.6298611111111,20.7538265306122), 'chid';
my $nknots = zeroes(longlong, 2);
my $t = zeroes( 2*$x->dim(0)+4, 2 );
my ($bcoef, $ndim, $kord) = chbs($x, $y, $g, 0, $nknots, $t);
is_pdl $nknots, longlong(24,24), 'chbs nknots';
is_pdl $t, pdl('0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 9 9; 0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 9 9'), 'chbs t';
is_pdl $bcoef, pdl('43.3 43.3 43.8 44.8 46.05 48.55 50.355556 54.244444 56.675 61.925 65 71.6 75.327778 83.272222 87.657143 96.942857 101.9875 112.6125 118.3 124.3; -23 -23 -22.583333 -21.416667 -18.410256 -11.589744 -4.3690476 12.369048 25.646259 56.353741 77.653509 126.34649 157.65749 228.34251 271.65991 368.34009 425.66149 552.33851 625.66667 706'), 'chbs bcoef';
is_pdl $ndim, longlong(20,20), 'chbs ndim';
is_pdl $kord, longlong(4,4), 'chbs kord';
my $x_slice = $x->slice('*1,0:-2'); # because calling with last value is out of range
my ($val) = bvalu($t, $bcoef, 0, $x_slice);
is_pdl $val->t, pdl('43.3 44.3 47.3 52.3 59.3 68.3 79.3 92.3 107.3; -23 -22 -15 4 41 102 193 320 489'), 'bvalu';

done_testing;
