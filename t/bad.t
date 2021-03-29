# -*-perl-*-
#
# test bad value handling in PDL
# - as it's a compile-time option we
#   skip unless $PDL::Config{WITH_BADVAL}
#

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
    plan tests => 82;
} else {
    # reduced testing
    plan tests => 10;

    my $x = pdl(1,2,3);
    is( $x->badflag(), 0 ); # 1
    
    my $y = pdl(4,5,6);
    my $c = $x + $y;
    is( $c->badflag(), 0 ); # 2
    is( $c->sum(), 21 );    # 3
    
    # can not set the bad flag
    $x->badflag(1);
    is( $x->badflag(), 0 );

    # and piddles do not have a bad value
    ok( ! defined $x->badvalue );

    # can not change a piddle to include bad values
    ok( all( ($x->setbadif( $x == 2 ) - pdl(1,2,3)) == 0 ) );

    $x = ones(3,2,4);
    $y = zeroes(2,4);
    $c = ones(2,4) * 3;
    is( $x->nbad, 0 );
    is( $x->ngood, 24 );
    ok( all( ($x->nbadover  - $y) == 0 ) );
    ok( all( ($x->ngoodover - $c) == 0 ) );

    exit;
}

my $usenan = $PDL::Config{BADVAL_USENAN} || 0;
my $perpdl = $PDL::Config{BADVAL_PER_PDL} || 0;

# check default behaviour (ie no bad data)
# - probably overkill
#
my $x = pdl(1,2,3);
is( $x->badflag(), 0, "no badflag" );

my $y = pdl(4,5,6);
my $c = $x + $y;
is( $c->badflag(), 0, "badflag not set in a copy" );
is( $c->sum(), 21, "sum() works on non bad-flag piddles" );

# is the flag propagated?
$x->badflag(1);
ok( $x->badflag(), "bad flag is now set" );

$c = $x + $y;
ok( $c->badflag(), "bad flag is propagated" );
is( $c->sum(), 21, "sum is still 21 with badflag set" );

$x->badflag(0);
$y->badflag(1);
$c = $x + $y;
ok( $c->badflag(), "badflag propagates on rhs of 'x+y'" );

# how about copies/vaffines/whatever
$x = rvals( long, 7, 7, {Centre=>[2,2]} );
$y = $x;
is( $y->badflag, 0, "badflag not set in a copy" );

$x->badflag(1);
$y = $x;
ok( $y->badflag, "badflag is now set in a copy" );

$x->badflag(0);
$y = $x->slice('2:5,3:4');
$c = $y->slice('0:1,(0)'); 
is( $y->badflag, 0, "slice handling okay with no badflag" );

$x->badflag(1);

my $i = "Type: %T Dim: %-15D State: %5S  Dataflow: %F";
print "Info: x = ", $x->info($i), "\n";
print "Info: y = ", $y->info($i), "\n";
print "Info: c = ", $c->info($i), "\n";

# let's check that it gets through to a child of a child
ok( $c->badflag, "badflag propagated throufh to a child" );

# can we change bad values
is( byte->badvalue, byte->orig_badvalue, "byte bad value is set to the default value" );
byte->badvalue(23);
is( byte->badvalue, 23, "changed bad value for byte" );
byte->badvalue( byte->orig_badvalue );

# check setbadat()
$x = pdl(1,2,3,4,5);
$x->setbadat(2);
is( PDL::Core::string($x), "[1 2 BAD 4 5]", "setbadat worked" );

# now check that badvalue() changes the piddle
# (only for integer types)
$x = convert($x,ushort);
my $badval = $x->badvalue;
$x->badvalue(44);
is( PDL::Core::string($x), "[1 2 BAD 4 5]", "changed badvalue" );
$x->badflag(0);
is( PDL::Core::string($x), "[1 2 44 4 5]", "can remove the bad value setting" );

# restore the bad value
$x->badvalue($badval);

$x = byte(1,2,3);
$y = byte(1,byte->badvalue,3);
$x->badflag(1);
$y->badflag(1);

# does string work?
# (this has implicitly been tested just above)
#
is( PDL::Core::string($y), "[1 BAD 3]", "can convert bad values to a string" );

# does addition work
$c = $x + $y;
is( sum($c), 8, "addition propagates the bad value" );

# does conversion of bad types work
$c = float($y);
ok( $c->badflag, "type conversion retains bad flag" );
is( PDL::Core::string($c), "[1 BAD 3]", "  and the value" );
is( sum($c), 4, "  and the sum" );

$x = byte(1,2,byte->badvalue,byte->badvalue,5,6,byte->badvalue,8,9);
$x->badflag(1);

is( PDL::Core::string($x->isbad),  "[0 0 1 1 0 0 1 0 0]", "isbad() works" );
is( PDL::Core::string($x->isgood), "[1 1 0 0 1 1 0 1 1]", "isgood() works" );

is( $x->nbad, 3, "nbad() works" );
is( $x->ngood, 6, "ngood() works" );

$x = byte( [255,255], [0,255], [0,0] );
$x->badflag(1);

is( PDL::Core::string($x->nbadover),  "[2 1 0]", "nbadover() works" );
is( PDL::Core::string($x->ngoodover), "[0 1 2]", "ngoodover() works" );

# check dataflow (or vaffine or whatever it's called)
$x = byte( [1,2,byte->badvalue,4,5], [byte->badvalue,0,1,2,byte->badvalue] );
$x->badflag(1);
$y = $x->slice(',(1)');
is( sum($y), 3, "sum of slice works" );
$y++;
is( PDL::Core::string($x),
    "\n[\n [  1   2 BAD   4   5]\n [BAD   1   2   3 BAD]\n]\n",
    "inplace addition of slice flows back to parent"
  );

$x = byte->badvalue * ones(byte,3,2);
is( $x->get_datatype, 0, "datatype remains a byte" );
$x->badflag(1);
is( PDL::Core::string( PDL::zcover($x) ), "[BAD BAD]", "zcover() okay" );
$x->set(1,1,1);
$x->set(2,1,1);
is( PDL::Core::string( PDL::zcover($x) ), "[BAD 0]", "  and still okay" );

# 255 is the default bad value for a byte array
#
$x = byte(1,2,255,4,5);
is( $x->median, 4, "median() works on good piddle" );
$x->badflag(1);
is( $x->median, 3, "median() works on bad biddle" );

# as random() creates numbers between 0 and 1 it won't
# accidentally create a bad value by chance (the default
# bad value for a double is either a very negative
# number or NaN).
#
$x = random(20);
$x->badflag(1);
is( $x->check_badflag, 0, "check_badflag did not find a bad value" );


# check out stats, since it uses several routines
# and setbadif
$x = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$y = $x->setbadif( $x < 20 ); 
my @s = $y->stats();                     
ok( approx( $s[0], 61.9375, ABSTOL ), "setbadif/stats test 1" );
ok( approx( $s[1], 27.6079, ABSTOL ), "setbadif/stats test 2" );
is( $s[2], 66.5, "setbadif/stats test 3" );
is( $s[3], 22, "setbadif/stats test 4" );  
is( $s[4], 98, "setbadif/stats test 5" );  
ok( approx( $s[6], 26.7312, ABSTOL ), "setbadif/stats test 6" );

# how about setbadtoval (was replacebad)
$x = $y->setbadtoval(20) - pdl(qw(42 47 98 20 22 96 74 41 79 76 96 20 32 76 25 59 20 96 32 20));
ok( all($x == 0), "setbadtoval() worked" );

# and inplace?
$x = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$y = $x->setbadif( $x < 20 ); 
$y->inplace->setbadtoval(20);
$x = $y - pdl(qw(42 47 98 20 22 96 74 41 79 76 96 20 32 76 25 59 20 96 32 20));
ok( all($x == 0), "   and inplace" );

# ditto for copybad
$x = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$y = $x->setbadif( $x < 20 ); 
$c = copybad( $x, $y );
is( PDL::Core::string( $c->isbad ), 
    "[0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1]",
  "isbad() worked" );

$x = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$y = $x->setbadif( $x < 20 ); 
$x->inplace->copybad( $y );
is( PDL::Core::string( $x->isbad ), 
    "[0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1]",
  "  and inplace" );

## $x->inplace->setbadif( $x % 2 ) does NOT work because
## ($x % 2) is performed inplace - ie the flag is set for 
## that function
#
##$x = sequence(3,3);
##$x->inplace->setbadif( $x % 2 );
###$x = $x->setbadif( $x % 2 );              # for when not bothered about inplace
##ok( PDL::Core::string( $x->clump(-1) ), 
##    "[0 BAD 2 BAD 4 BAD 6 BAD 8]" );   #

## look at propagation of bad flag using inplace routines...
$x = sequence( byte, 2, 3 );
$x = $x->setbadif( $x == 3 );
$y = $x->slice("(1),:");
$x->inplace->setbadtoval(3);
is( $y->badflag, 0, "badflag cleared using inplace setbadtoval()" );

$x = sequence( byte, 2, 3 );
$y = $x->slice("(1),:");
my $mask = sequence( byte, 2, 3 );
$mask = $mask->setbadif( ($mask % 3) == 2 );
print "x,y == ", $x->badflag, ",", $y->badflag, "\n";
$x->inplace->copybad( $mask );
print "x,y == ", $x->badflag, ",", $y->badflag, "\n";
print "$x $y\n";
is( $y->badflag, 1, "badflag propagated using inplace copybad()" );

# test some of the qsort functions
$x = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$y = $x->setbadif( $x < 20 ); 
my $ix = qsorti( $y );
is( PDL::Core::string( $y->index($ix) ), 
    "[22 25 32 32 41 42 47 59 74 76 76 79 96 96 96 98 BAD BAD BAD BAD]",
    "qsorti() okay"
    );                                   # 

# check comparison/bit operators in ops.pd

$x = pdl( 2, 4, double->badvalue );
$x->badflag(1);
$y = abs( $x - pdl(2.001,3.9999,234e23) ) > 0.01;
is( PDL::Core::string( $y ), "[0 0 BAD]", "abs() and >" );

$y = byte(1,2,byte->badvalue,4);
$y->badflag(1);
is( PDL::Core::string( $y << 2 ), "[4 8 BAD 16]", "<<" );

$x = pdl([1,2,3]);
$x->badflag(1);
$y = $x->assgn;
is( $y->badflag, 1, "assgn propagated badflag");
$x->badflag(0);
is( $y->badflag, 1, "assgn is not a deep copy for the badflag");

# check that at and sclr return the correct values
TODO: {
   $x = pdl q[BAD];
   local $TODO = 'check that at and sclr return correct values and the same';

   is( PDL::Core::string($x), 'BAD', 'can convert PDL to string' );
   is( $x->at, 'BAD', 'at() returns BAD for a bad value' );
   is( $x->sclr, 'BAD', 'sclr() returns BAD for a bad value' );
}

# quick look at math.pd
use PDL::Math;

$x = pdl(0.5,double->badvalue,0);
$x->badflag(1);
$y = bessj0($x);
is( PDL::Core::string( isbad($y) ), "[0 1 0]", "bessj0()" );

$x = pdl(double->badvalue,0.8);
$x->badflag(1);
$y = bessjn($x,3);  # thread over n()
is( PDL::Core::string( isbad($y) ), "[1 0]", "thread over bessjn()" );
ok( abs($y->at(1)-0.010) < 0.001 );

$x = pdl( 0.01, 0.0 );
$x->badflag(1);
ok( all( abs(erfi($x)-pdl(0.00886,0)) < 0.001 ), "erfi()" );

# I haven't changed rotate, but it should work anyway
$x = byte( 0, 1, 2, 4, 5 );
$x->setbadat(2);
is( PDL::Core::string( $x->rotate(2) ), "[4 5 0 1 BAD]", "rotate()" );

# check norm
$x = float( 2, 0, 2, 2 )->setvaltobad(0.0);
$y = $x->norm;
$c = $x/sqrt(sum($x*$x));
ok( all( approx( $y, $c, ABSTOL ) ), "norm()" );

# Image2D
my $ans = pdl(
 [ 3,  7, 11, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  5, 13, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27]
);

$x = xvals zeroes 10,3;
$x->setbadat(2,1);

$y = pdl [1,2],[2,1];

use PDL::Image2D;
$c = conv2d($x, $y);

is( int(at(sum($c-$ans))), 0, "conv2d()" ); 

$x = zeroes(5,5);
$x->badflag(1);
my $t = $x->slice("1:3,1:3");
$t .= ones(3,3);
$x->setbadat(2,2);

$y = sequence(3,3);
$ans = pdl ( [0,0,0,0,0],[0,0,2,0,0],[0,1,5,2,0],[0,0,4,0,0],[0,0,0,0,0]);
my $m = med2d($x,$y);
my $m_sub = $m-$ans;
my $m_sum = sum($m_sub);
my $m_at = at($m_sum);
my $m_int = int($m_at);
is( $m_int, 0, "med2d()" ) or diag "x: $x\nm: $m\nans: $ans\nm_sub: $m_sub\nm_sum: $m_sum\nm_at: $m_at\nm_int: $m_int";

# propagation of badflag using inplace ops (ops.pd)

# test biop fns
$x = sequence(3,3);
$c = $x->slice(',(1)');
$y = $x->setbadif( $x % 2 );
$x->inplace->plus($y,0);
print $x;
print "$c\n";
is( PDL::Core::string($c), "[BAD 8 BAD]", "inplace biop - plus()" );

# test bifunc fns
$x = sequence(3,3);
$c = $x->slice(',(1)');
$y = $x->setbadif( $x % 3 != 0 );
$x->inplace->power($y,0);
print $x;
print "$c\n";
is( PDL::Core::string($c), "[27 BAD BAD]", "inplace bifunc - power()" );

# test histogram (using hist)
$x = pdl( qw/1 2 3 4 5 4 3 2 2 1/ );
$x->setbadat(1);
$y = hist $x, 0, 6, 1;
print "values:    $x\n";
print "histogram: $y\n";
is( PDL::Core::string($y), "[0 2 2 2 2 1]", "hist()" );

#$y = $x->isfinite;
#print "isfinite(X): datatype = [",$y->get_datatype,"]\n";

$x->inplace->isfinite;
#print "X: datatype = [",$x->get_datatype,"]\n";
is( PDL::Core::string($x), "[1 0 1 1 1 1 1 1 1 1]", "isfinite()" );
#print "X: datatype = [",$x->get_datatype,"]\n";

# histogram2d
$x = long(1,1,1,2,2);
$y = long(2,1,1,1,1);
$y->setbadat(0);
my @c = ( 1,0,3 );
$c = histogram2d($x,$y,@c,@c);
is( PDL::Core::string($c->clump(-1)), 
    "[0 0 0 0 2 2 0 0 0]",
  "histogram2d()" );

# weird propagation of bad values
# - or is it?
#
#$x = sequence( byte, 2, 3 );
#$x = $x->setbadif( $x == 3 );
#$y = $x->slice("(1),:");
#$x .= $x->setbadtoval(3);
#ok( $x->badflag, 0 );                  # this fails
#ok( $y->badflag, 0 );                  # as does this

# badmask: inplace
$x = sequence(5);
$x->setbadat(2);
$x->inplace->badmask(0);
is( PDL::Core::string($x), "[0 1 0 3 4]", "inplace badmask()" );

# setvaltobad
$x = sequence(10) % 4;
$x->inplace->setvaltobad( 1 );
like( PDL::Core::string( $x->clump(-1) ), 
    qr{^\[-?0 BAD 2 3 -?0 BAD 2 3 -?0 BAD]$}, "inplace setvaltobad()" );

# check setvaltobad for non-double piddles
my $fa = pdl( float,  1..4) / 3;
my $da = pdl( double, 1..4) / 3;
ok( all($fa->setvaltobad(2/3)->isbad == $da->setvaltobad(2/3)->isbad), "setvaltobad for float piddle");

# simple test for setnantobad
# - could have a 1D FITS image containing
#   NaN's and then a simple version of rfits
#   (can't use rfits as does conversion!)
$x->inplace->setnantobad;
like( PDL::Core::string( $x->clump(-1) ), 
    qr{^\[-?0 BAD 2 3 -?0 BAD 2 3 -?0 BAD]$}, "inplace setnantobad()" );

# test r/wfits
use PDL::IO::FITS;

$x = sequence(10)->setbadat(0);
print "Writing to fits: $x  type = (", $x->get_datatype, ")\n";
$x->wfits($fname);
$y = rfits($fname);
print "Read from fits:  $y  type = (", $y->get_datatype, ")\n";

ok( $y->slice('0:0')->isbad, "rfits/wfits propagated bad flag" );
ok( sum(abs($x-$y)) < 1.0e-5, "  and values" );

# now force to integer
$x->wfits($fname,16);
$y = rfits($fname);
print "BITPIX 16: datatype == ", $y->get_datatype, " badvalue == ", $y->badvalue(), "\n";
ok( $y->slice('0:0')->isbad, "wfits coerced bad flag with integer datatype" );
ok( sum(abs(convert($x,short)-$y)) < 1.0e-5, "  and the values" );

# check that we can change the value used to represent
# missing elements for floating points (earlier tests only did integer types)
# IF we are not using NaN's
#
SKIP: {
    skip( "Skipped: test only valid when not using NaN's as bad values", 2 )
      if $usenan;

    # perhaps should check that the value can't be changed when NaN's are
    # being used.
    #

    is( float->badvalue, float->orig_badvalue, "default bad value for floats matches" );
    is( float->badvalue(23), 23, "changed floating-point bad value" );
    float->badvalue( float->orig_badvalue );
}

SKIP: {

    skip ("Skipped: test only valid when enabling bad values per pdl", 3)
      unless $perpdl;

    $x = sequence(4);
    $x->badvalue(3);
    $x->badflag(1);
    $y = $x->slice('2:3');
    is( $y->badvalue, 3, "can propagate per-piddle bad value");
    is( $y->sum, 2, "and the propagated value is recognised as bad");

    $x = sequence(4);
    is ($x->badvalue, double->orig_badvalue, "no long-term affects of per-piddle changes [1]");

}

SKIP: {
    skip ("Skipped: test not valid if per-piddle bad values are used", 1)
      if $perpdl;

    $x = double(4);
    double->badvalue(3);
    is($x->badvalue, double->badvalue, "no long-term affects of per-piddle changes [2]");
    double->badvalue(double->orig_badvalue);

}

# At the moment we do not allow per-piddle bad values
# and the use of NaN's.
#TODO: {
#    local $TODO = "Need to work out whan NaN and per-piddle bad values means";
#    is (0, 1);
#}

# end
