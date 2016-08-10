# -*-perl-*-
#

use Test::More;
BEGIN {
    plan tests => 29;
}

use PDL;
use PDL::Image2D;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

use strict;

sub tapprox {
    my $a = shift;
    my $b = shift;
    my $text = shift;
    my $d = abs($a - $b);
    ok( all($d < 1.0e-5),$text );
}

# Right answer

my $ans = pdl(
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27]
);

# conv2d
my $a = xvals zeroes 10,3;
my $b = pdl [1,2],[2,1];
my $c = conv2d ($a, $b);
tapprox( $c, $ans ,"conv2d xvals");  # 1

# conv2d
$a=zeroes(3,3); 
$a->set(1,1,1);
$b = sequence(3,3);
tapprox( conv2d($a,$b), $b, "conv2d trivial kernel" );    # 2

# conv2d: boundary => reflect
$a=ones(3,3);  
$ans = pdl ([12,18,24],[30,36,42],[48,54,60]);
tapprox( conv2d($b,$a,{Boundary => 'Reflect'}), $ans, "conv2d reflect");  #3

# conv2d: boundary => replicate
$ans = pdl ([12,18,24],[30,36,42],[48,54,60]);
tapprox( conv2d($b,$a,{Boundary => 'Replicate'}), $ans, "conv2d replicate" ); #4

# conv2d: boundary => truncate
$ans = pdl ([8,15,12],[21,36,27],[20,33,24]);
tapprox( conv2d($b,$a,{Boundary => 'Truncate'}), $ans, "conv2d truncate" ); #5

# max2d_ind
$a = 100 / (1.0 + rvals(5,5));
$a = $a * ( $a < 90 );
my @ans = $a->max2d_ind();
print "max2d_ind: " . join( ',', @ans ) . "\n";
ok( ($ans[0] == 50) & ($ans[1] == 1) & ($ans[2] == 2), "max2d_ind" );

# centroid2d
$a = 100.0 / rvals( 20, 20, { Centre => [ 8, 12.5 ] } );
$a = $a * ( $a >= 9 );
@ans = $a->centroid2d( 10, 10, 20 );
tapprox( $ans[0], 8.432946, "centroid2d (0)" );  # numbers calculated by an independent program
tapprox( $ans[1], 11.756724, "centroid2d (1)" );

# med2d
my $t;
$a = zeroes(5,5);
$t = $a->slice("1:3,1:3");
$t .= ones(3,3);
$b = sequence(3,3);
$ans = pdl ( [0,0,0,0,0],[0,0,1,0,0],[0,1,4,2,0],[0,0,4,0,0],[0,0,0,0,0]);
tapprox( med2d($a,$b), $ans, "med2d" );

# med2df
$a = sequence(10,10);
tapprox( med2df($a,3,3,{Boundary=>'Truncate'})->slice("1:-2,1:-2"), $a->slice("1:-2,1:-2"), "med2df");

# patch2d
$a = ones(5,5);
my $mask = zeroes(5,5);
$mask->set(2,2,1);
tapprox( patch2d($a,$mask), $a, "patch2d 1-element no-op");  # 6

# note: 
#   with no bad values, any bad pixel which has no good neighbours
#   has its value copied
# 
my $m = $mask->slice('1:3,1:3');
$m .= 1;
$ans = $a->copy;
print $a, $mask, patch2d($a,$mask);
tapprox( patch2d($a,$mask), $ans, "patch2d 2d slice no-op" );  # 7

if ( $PDL::Bad::Status ) {
    # patchbad2d: bad data
    $m = $a->slice('1:3,1:3');
    $m .= $a->badvalue;
    $a->badflag(1);        # should sort out propagation of badflag

    $ans = ones(5,5);
    $ans->set(2,2,$ans->badvalue);
    $ans->badflag(1);

    #print $a, patchbad2d($a);
    tapprox( patchbad2d($a), $ans, "patchbad2d" );  # 8

    # patchbad2d: good data
    $a = sequence(5,5);
    #print $a, patchbad2d($a);
    tapprox( patchbad2d($a), $a, "patchbad2d good data" );  # 9

    # max2d_ind
    $a = 100 / (1.0 + rvals(5,5));
    $a = $a->setbadif( $a > 90 );
    my @ans = $a->max2d_ind();
    print "max2d_ind: " . join( ',', @ans ) . "\n";
    ok( ($ans[0] == 50) & ($ans[1] == 1) & ($ans[2] == 2), "max2d_ind bad data");

    # centroid2d
    $a = 100.0 / rvals( 20, 20, { Centre => [ 8, 12.5 ] } );
    $a = $a->setbadif( $a < 9 );
    @ans = $a->centroid2d( 10, 10, 20 );
    tapprox( $ans[0], 8.432946, "centroid2d bad data (0)");  # numbers should be same as when set < 9 to 0
    tapprox( $ans[1], 11.756724, "centroid2d bad data (1)");

} else { 
    my $msg = "PDL::Bad support not available.";
    for (0..4) { skip($msg,1,1) } # skip 5 tests
}
# box2d bug test
my $one = random(10,10);
my $box = cat $one,$one,$one;

my $bav = $one->box2d(3,3,0);
my $boxav = $box->box2d(3,3,0);

# all 2D averages should be the same
tapprox($bav->sum,$boxav->clump(2)->sumover, "box2d");

# cc8compt & cc4compt
$a = pdl([0,1,1,0,1],[1,0,1,0,0],[0,0,0,1,0],[1,0,0,0,0],[0,1,0,1,1]);
ok(cc8compt($a)->max == 4, "cc8compt");
ok(cc4compt($a)->max == 7, "cc4compt");
eval 'ccNcompt($a,5)';
ok($@ ne '', "ccNcompt(5) fails");
eval 'ccNcompt($a,8)';
ok($@ eq '', "ccNcompt(8) succeeds");
my $im = (xvals(25,25)+yvals(25,25));
my $seg_b = cc4compt(byte $im%2);
ok($seg_b->type >= long, "cc4compt type>=long");
ok(cc4compt($im%2)->max == $seg_b->max, "cc4compt results don't wrap for byte type");

# pnpoly
my $px = pdl(0,3,1);
my $py = pdl(0,1,4);
my $im = zeros(5,5);
my $im2 = zeroes(5,5);
my $x = $im->xvals;
my $y = $im->yvals;
my $ps = $px->cat($py)->xchg(0,1);
my $im_mask = pnpoly($x,$y,$px,$py);
ok(sum($im_mask) == 5, "pnpoly old style, 5 pixels inside");
my $inpixels = pdl q[ 1 1 ; 1 2 ; 1 3 ; 2 1 ; 2 2 ];
ok(sum($inpixels - qsortvec(scalar whichND($im_mask))) == 0, "pnpoly old style, correct pixels inside");

# Make sure the PDL pnpoly and the PP pnpoly give the same result
ok(all($im_mask == $im->pnpoly($ps)), "pnpoly old style vs new style");

# Trivial test to make sure the polyfills using the pnpoly algorithm are working
$im .= 0;
polyfillv($im2,$ps,{'Method'=>'pnpoly'}) .= 22;
ok(all(polyfill($im,$ps,22,{'Method'=>'pnpoly'}) == $im2), "polyfill using pnpoly algorithm");


# Trivial test to make sure the polyfills are working
$im .= 0;
$im2 .= 0;
polyfillv($im2,$ps) .= 25;
polyfill($im,$ps,25);
ok(all($im == $im2), "polyfill using default algorithm");
