# -*-perl-*-
#

use Test;
BEGIN { 
    plan tests => 9; 
}

use PDL;
use PDL::Image2D;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

use strict;

sub approx {
    my $a = shift;
    my $b = shift;
    my $d = abs($a - $b);
    ok( all($d < 1.0e-5) );
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
approx( $c, $ans );  # 1

# conv2d
$a=zeroes(3,3); 
$a->set(1,1,1);
$b = sequence(3,3);
approx( conv2d($a,$b), $b );    # 2

# conv2d: boundary => reflect
$a=ones(3,3);  
$ans = pdl ([12,18,24],[30,36,42],[48,54,60]);
approx( conv2d($b,$a,{Boundary => 'Reflect'}), $ans );

# conv2d: boundary => truncate
$ans = pdl ([8,15,12],[21,36,27],[20,33,24]);
approx( conv2d($b,$a,{Boundary => 'Truncate'}), $ans );

# med2d
my $t;
$a = zeroes(5,5);
$t = $a->slice("1:3,1:3");
$t .= ones(3,3);
$b = sequence(3,3);
$ans = pdl ( [0,0,0,0,0],[0,0,1,0,0],[0,1,4,2,0],[0,0,4,0,0],[0,0,0,0,0]);
approx( med2d($a,$b), $ans );

# patch2d
$a = ones(5,5);
my $mask = zeroes(5,5);
$mask->set(2,2,1);
approx( patch2d($a,$mask), $a );  # 6

# note: 
#   with no bad values, any bad pixel which has no good neighbours
#   has its value copied
# 
my $m = $mask->slice('1:3,1:3');
$m .= 1;
$ans = $a->copy;
print $a, $mask, patch2d($a,$mask);
approx( patch2d($a,$mask), $ans );  # 7

if ( $PDL::Bad::Status ) {
    # patchbad2d: bad data
    $m = $a->slice('1:3,1:3');
    $m .= $a->badvalue;
    $a->badflag(1);        # should sort out propogation of badflag

    $ans = ones(5,5);
    $ans->set(2,2,$ans->badvalue);
    $ans->badflag(1);

    #print $a, patchbad2d($a);
    approx( patchbad2d($a), $ans );  # 8
    
    # patchbad2d: good data
    $a = sequence(5,5);
    #print $a, patchbad2d($a);
    approx( patchbad2d($a), $a );  # 9

} else { 
    skip(1,1,1);
    skip(1,1,1);
}
