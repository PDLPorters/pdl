# -*-perl-*-
#
# test bad value handling in PDL
# - as it's a compile-time option we
#   skip unless $PDL::Config{WITH_BADVAL}

use strict;
use Test::More;

# although approx() caches the tolerance value, we
# use it in every call just to document things
#
use constant ABSTOL => 1.0e-4;

use File::Temp qw( tempfile );
my $fname;
{
   local $^W = 0;
   (undef, $fname) = tempfile( 'delmeXXXXX', SUFFIX => '.fits', OPEN => 0 );
}

END {
    unlink $fname if -e $fname;
}

use PDL::LiteF;
$| = 1;

use PDL::Config;
if ( $PDL::Config{WITH_BADVAL} ) {
    plan tests => 2;
} else {
    # reduced testing
    plan tests => 10;

    my $a = pdl(1,2,3);
    is( $a->badflag(), 0 ); # 1
    
    my $b = pdl(4,5,6);
    my $c = $a + $b;
    is( $c->badflag(), 0 ); # 2
    is( $c->sum(), 21 );    # 3
    
    # can not set the bad flag
    $a->badflag(1);
    is( $a->badflag(), 0 );

    # and piddles do not have a bad value
    ok( ! defined $a->badvalue );

    # can not change a piddle to include bad values
    ok( all( ($a->setbadif( $a == 2 ) - pdl(1,2,3)) == 0 ) );

    $a = ones(3,2,4);
    $b = zeroes(2,4);
    $c = ones(2,4) * 3;
    is( $a->nbad, 0 );
    is( $a->ngood, 24 );
    ok( all( ($a->nbadover  - $b) == 0 ) );
    ok( all( ($a->ngoodover - $c) == 0 ) );

    exit;
}

# Image2D
my $ans = pdl(
 [ 3,  7, 11, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  5, 13, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27]
);

my $a = xvals zeroes 10,3;
$a->setbadat(2,1);

my $b = pdl [1,2],[2,1];

use PDL::Image2D;
my $c = conv2d($a, $b);

is( int(at(sum($c-$ans))), 0, "conv2d()" ); 

$a = zeroes(5,5);
$a->badflag(1);
my $t = $a->slice("1:3,1:3");
$t .= ones(3,3);
$a->setbadat(2,2);

$b = sequence(3,3);
$ans = pdl ( [0,0,0,0,0],[0,0,2,0,0],[0,1,5,2,0],[0,0,4,0,0],[0,0,0,0,0]);
is( int(at(sum(med2d($a,$b)-$ans))), 0, "med2d()" );
