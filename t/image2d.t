use Test;

plan tests => 5;

use PDL;
use PDL::Image2D;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

use strict;

# Right answer

my $ans = pdl(
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27]
);

my $a = xvals zeroes 10,3;

my $b = pdl [1,2],[2,1];

my $c=conv2d ($a, $b);

ok( int(at(sum($c-$ans))), 0 ); # 1

$a=zeroes(3,3); $a->set(1,1,1);
$b = sequence(3,3);

ok( int(at(sum(conv2d($a,$b)-$b))), 0 );

$a=ones(3,3);  $ans = pdl ([12,18,24],[30,36,42],[48,54,60]);
ok( int(at(sum(conv2d($b,$a,{Boundary => 'Reflect'})-$ans))), 0 );

$ans = pdl ([8,15,12],[21,36,27],[20,33,24]);
ok( int(at(sum(conv2d($b,$a,{Boundary => 'Truncate'})-$ans))), 0 );

my $t;
$a=zeroes(5,5);$t=$a->slice("1:3,1:3");$t.=ones(3,3);$b=sequence(3,3);
$ans = pdl ( [0,0,0,0,0],[0,0,1,0,0],[0,1,4,2,0],[0,0,4,0,0],[0,0,0,0,0]);
ok( int(at(sum(med2d($a,$b)-$ans))), 0 );

