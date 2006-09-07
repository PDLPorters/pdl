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

my $fname = 'delme.fits';

END {
    unlink $fname if -e $fname;
}

use PDL::LiteF;
$| = 1;

use PDL::Config;
if ( $PDL::Config{WITH_BADVAL} ) {
    plan tests => 78;
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

my $usenan = $PDL::Config{BADVAL_USENAN} || 0;
my $perpdl = $PDL::Config{BADVAL_PER_PDL} || 0;

# check default behaviour (ie no bad data)
# - probably overkill
#
my $a = pdl(1,2,3);
is( $a->badflag(), 0, "no badflag" );

my $b = pdl(4,5,6);
my $c = $a + $b;
is( $c->badflag(), 0, "badflag not set in a copy" );
is( $c->sum(), 21, "sum() works on non bad-flag piddles" );

# is the flag propogated?
$a->badflag(1);
ok( $a->badflag(), "bad flag is now set" );

$c = $a + $b;
ok( $c->badflag(), "bad flag is propogated" );
is( $c->sum(), 21, "sum is still 21 with badflag set" );

$a->badflag(0);
$b->badflag(1);
$c = $a + $b;
ok( $c->badflag(), "badflag propogates on rhs of 'a+b'" );

# how about copies/vaffines/whatever
$a = rvals( long, 7, 7, {Centre=>[2,2]} );
$b = $a;
is( $b->badflag, 0, "badflag not set in a copy" );

$a->badflag(1);
$b = $a;
ok( $b->badflag, "badflag is now set in a copy" );

$a->badflag(0);
$b = $a->slice('2:5,3:4');
$c = $b->slice('0:1,(0)'); 
is( $b->badflag, 0, "slice handling okay with no badflag" );

$a->badflag(1);

my $i = "Type: %T Dim: %-15D State: %5S  Dataflow: %F";
print "Info: a = ", $a->info($i), "\n";
print "Info: b = ", $b->info($i), "\n";
print "Info: c = ", $b->info($i), "\n";

# let's check that it gets through to a child of a child
ok( $c->badflag, "badflag propogated throufh to a child" );

# can we change bad values
is( byte->badvalue, byte->orig_badvalue, "byte bad value is set to the default value" );
byte->badvalue(23);
is( byte->badvalue, 23, "changed bad value for byte" );
byte->badvalue( byte->orig_badvalue );

# check setbadat()
$a = pdl(1,2,3,4,5);
$a->setbadat(2);
is( PDL::Core::string($a), "[1 2 BAD 4 5]", "setbadat worked" );

# now check that badvalue() changes the piddle
# (only for integer types)
$a = convert($a,ushort);
my $badval = $a->badvalue;
$a->badvalue(44);
is( PDL::Core::string($a), "[1 2 BAD 4 5]", "changed badvalue" );
$a->badflag(0);
is( PDL::Core::string($a), "[1 2 44 4 5]", "can remove the bad value setting" );

# restore the bad value
$a->badvalue($badval);

$a = byte(1,2,3);
$b = byte(1,byte->badvalue,3);
$a->badflag(1);
$b->badflag(1);

# does string work?
# (this has implicitly been tested just above)
#
is( PDL::Core::string($b), "[1 BAD 3]", "can convert bad values to a string" );

# does addition work
$c = $a + $b;
is( sum($c), 8, "addition propogates the bad value" );

# does conversion of bad types work
$c = float($b);
ok( $c->badflag, "type conversion retains bad flag" );
is( PDL::Core::string($c), "[1 BAD 3]", "  and the value" );
is( sum($c), 4, "  and the sum" );

$a = byte(1,2,byte->badvalue,byte->badvalue,5,6,byte->badvalue,8,9);
$a->badflag(1);

is( PDL::Core::string($a->isbad),  "[0 0 1 1 0 0 1 0 0]", "isbad() works" );
is( PDL::Core::string($a->isgood), "[1 1 0 0 1 1 0 1 1]", "isgood() works" );

is( $a->nbad, 3, "nbad() works" );
is( $a->ngood, 6, "ngood() works" );

$a = byte( [255,255], [0,255], [0,0] );
$a->badflag(1);

is( PDL::Core::string($a->nbadover),  "[2 1 0]", "nbadover() works" );
is( PDL::Core::string($a->ngoodover), "[0 1 2]", "ngoodover() works" );

# check dataflow (or vaffine or whatever it's called)
$a = byte( [1,2,byte->badvalue,4,5], [byte->badvalue,0,1,2,byte->badvalue] );
$a->badflag(1);
$b = $a->slice(',(1)');
is( sum($b), 3, "sum of slice works" );
$b++;
is( PDL::Core::string($a),
    "\n[\n [  1   2 BAD   4   5]\n [BAD   1   2   3 BAD]\n]\n",
    "inplace addition of slice flows back to parent"
  );

$a = byte->badvalue * ones(byte,3,2);
is( $a->get_datatype, 0, "datatype remains a byte" );
$a->badflag(1);
is( PDL::Core::string( zcover($a) ), "[BAD BAD]", "zcover() okay" );
$a->set(1,1,1);
$a->set(2,1,1);
is( PDL::Core::string( zcover($a) ), "[BAD 0]", "  and still okay" );

# 255 is the default bad value for a byte array
#
$a = byte(1,2,255,4,5);
is( $a->median, 4, "median() works on good piddle" );
$a->badflag(1);
is( $a->median, 3, "median() works on bad biddle" );

# as random() creates numbers between 0 and 1 it won't
# accidentally create a bad value by chance (the default
# bad value for a double is either a very negative
# number or NaN).
#
$a = random(20);
$a->badflag(1);
is( $a->check_badflag, 0, "check_badflag did not find a bad value" );

$i = "Type: %T Dim: %-15D State: %5S  Dataflow: %F";

# check out stats, since it uses several routines
# and setbadif
$a = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$b = $a->setbadif( $a < 20 ); 
my @s = $b->stats();                     
ok( approx( $s[0], 61.9375, ABSTOL ), "setbadif/stats test 1" );
ok( approx( $s[1], 27.6079, ABSTOL ), "setbadif/stats test 2" );
is( $s[2], 66.5, "setbadif/stats test 3" );
is( $s[3], 22, "setbadif/stats test 4" );  
is( $s[4], 98, "setbadif/stats test 5" );  
ok( approx( $s[6], 26.7312, ABSTOL ), "setbadif/stats test 6" );

# how about setbadtoval (was replacebad)
$a = $b->setbadtoval(20) - pdl(qw(42 47 98 20 22 96 74 41 79 76 96 20 32 76 25 59 20 96 32 20));
ok( all($a == 0), "setbadtoval() worked" );

# and inplace?
$a = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$b = $a->setbadif( $a < 20 ); 
$b->inplace->setbadtoval(20);
$a = $b - pdl(qw(42 47 98 20 22 96 74 41 79 76 96 20 32 76 25 59 20 96 32 20));
ok( all($a == 0), "   and inplace" );

# ditto for copybad
$a = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$b = $a->setbadif( $a < 20 ); 
$c = copybad( $a, $b );
is( PDL::Core::string( $c->isbad ), 
    "[0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1]",
  "isbad() worked" );

$a = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$b = $a->setbadif( $a < 20 ); 
$a->inplace->copybad( $b );
is( PDL::Core::string( $a->isbad ), 
    "[0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1]",
  "  and inplace" );

## $a->inplace->setbadif( $a % 2 ) does NOT work because
## ($a % 2) is performed inplace - ie the flag is set for 
## that function
#
##$a = sequence(3,3);
##$a->inplace->setbadif( $a % 2 );
###$a = $a->setbadif( $a % 2 );              # for when not bothered about inplace
##ok( PDL::Core::string( $a->clump(-1) ), 
##    "[0 BAD 2 BAD 4 BAD 6 BAD 8]" );   #

## look at propogation of bad flag using inplace routines...
$a = sequence( byte, 2, 3 );
$a = $a->setbadif( $a == 3 );
$b = $a->slice("(1),:");
$a->inplace->setbadtoval(3);
is( $b->badflag, 0, "badflag cleared using inplace setbadtoval()" );

$a = sequence( byte, 2, 3 );
$b = $a->slice("(1),:");
my $mask = sequence( byte, 2, 3 );
$mask = $mask->setbadif( ($mask % 3) == 2 );
print "a,b == ", $a->badflag, ",", $b->badflag, "\n";
$a->inplace->copybad( $mask );
print "a,b == ", $a->badflag, ",", $b->badflag, "\n";
print "$a $b\n";
is( $b->badflag, 1, "badflag propogated using inplace copybad()" );

# test some of the qsort functions
$a = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$b = $a->setbadif( $a < 20 ); 
my $ix = qsorti( $b );
is( PDL::Core::string( $b->index($ix) ), 
    "[22 25 32 32 41 42 47 59 74 76 76 79 96 96 96 98 BAD BAD BAD BAD]",
    "qsorti() okay"
    );                                   # 

# check comparison/bit operators in ops.pd

$a = pdl( 2, 4, double->badvalue );
$a->badflag(1);
$b = abs( $a - pdl(2.001,3.9999,234e23) ) > 0.01;
is( PDL::Core::string( $b ), "[0 0 BAD]", "abs() and >" );

$b = byte(1,2,byte->badvalue,4);
$b->badflag(1);
is( PDL::Core::string( $b << 2 ), "[4 8 BAD 16]", "<<" );

$a = pdl([1,2,3]);
$a->badflag(1);
$b = $a->assgn;
is( $b->badflag, 1, "assgn propogated badflag");
$a->badflag(0);
is( $b->badflag, 1, "assgn is not a deep copy for the badflag");

# quick look at math.pd
use PDL::Math;

$a = pdl(0.5,double->badvalue,0);
$a->badflag(1);
$b = bessj0($a);
is( PDL::Core::string( isbad($b) ), "[0 1 0]", "bessj0()" );

$a = pdl(double->badvalue,0.8);
$a->badflag(1);
$b = bessjn($a,3);  # thread over n()
is( PDL::Core::string( isbad($b) ), "[1 0]", "thread over bessjn()" );
ok( abs($b->at(1)-0.010) < 0.001 );

$a = pdl( 0.01, 0.0 );
$a->badflag(1);
ok( all( abs(erfi($a)-pdl(0.00886,0)) < 0.001 ), "erfi()" );

# I haven't changed rotate, but it should work anyway
$a = byte( 0, 1, 2, 4, 5 );
$a->setbadat(2);
is( PDL::Core::string( $a->rotate(2) ), "[4 5 0 1 BAD]", "rotate()" );

# check norm
$a = float( 2, 0, 2, 2 )->setvaltobad(0.0);
$b = $a->norm;
$c = $a/sqrt(sum($a*$a));
ok( all( approx( $b, $c, ABSTOL ) ), "norm()" );

# Image2D
my $ans = pdl(
 [ 3,  7, 11, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  5, 13, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27]
);

$a = xvals zeroes 10,3;
$a->setbadat(2,1);

$b = pdl [1,2],[2,1];

use PDL::Image2D;
$c = conv2d($a, $b);

is( int(at(sum($c-$ans))), 0, "conv2d()" ); 

$a = zeroes(5,5);
$a->badflag(1);
my $t = $a->slice("1:3,1:3");
$t .= ones(3,3);
$a->setbadat(2,2);

$b = sequence(3,3);
$ans = pdl ( [0,0,0,0,0],[0,0,2,0,0],[0,1,5,2,0],[0,0,4,0,0],[0,0,0,0,0]);
is( int(at(sum(med2d($a,$b)-$ans))), 0, "med2d()" );

# propogation of badflag using inplace ops (ops.pd)

# test biop fns
$a = sequence(3,3);
$c = $a->slice(',(1)');
$b = $a->setbadif( $a % 2 );
$a->inplace->plus($b,0);
print $a;
print "$c\n";
is( PDL::Core::string($c), "[BAD 8 BAD]", "inplace biop - plus()" );

# test bifunc fns
$a = sequence(3,3);
$c = $a->slice(',(1)');
$b = $a->setbadif( $a % 3 != 0 );
$a->inplace->power($b,0);
print $a;
print "$c\n";
is( PDL::Core::string($c), "[27 BAD BAD]", "inplace bifunc - power()" );

# test histogram (using hist)
$a = pdl( qw/1 2 3 4 5 4 3 2 2 1/ );
$a->setbadat(1);
$b = hist $a, 0, 6, 1;
print "values:    $a\n";
print "histogram: $b\n";
is( PDL::Core::string($b), "[0 2 2 2 2 1]", "hist()" );

#$b = $a->isfinite;
#print "isfinite(A): datatype = [",$b->get_datatype,"]\n";

$a->inplace->isfinite;
#print "A: datatype = [",$a->get_datatype,"]\n";
is( PDL::Core::string($a), "[1 0 1 1 1 1 1 1 1 1]", "isfinite()" );
#print "A: datatype = [",$a->get_datatype,"]\n";

# histogram2d
$a = long(1,1,1,2,2);
$b = long(2,1,1,1,1);
$b->setbadat(0);
my @c = ( 1,0,3 );
$c = histogram2d($a,$b,@c,@c);
is( PDL::Core::string($c->clump(-1)), 
    "[0 0 0 0 2 2 0 0 0]",
  "histogram2d()" );

# weird propogation of bad values
# - or is it?
#
#$a = sequence( byte, 2, 3 );
#$a = $a->setbadif( $a == 3 );
#$b = $a->slice("(1),:");
#$a .= $a->setbadtoval(3);
#ok( $a->badflag, 0 );                  # this fails
#ok( $b->badflag, 0 );                  # as does this

# badmask: inplace
$a = sequence(5);
$a->setbadat(2);
$a->inplace->badmask(0);
is( PDL::Core::string($a), "[0 1 0 3 4]", "inplace badmask()" );

# setvaltobad
$a = sequence(10) % 4;
$a->inplace->setvaltobad( 1 );
is( PDL::Core::string( $a->clump(-1) ), 
    "[0 BAD 2 3 0 BAD 2 3 0 BAD]", "inplace setvaltobad()" );

# simple test for setnantobad
# - could have a 1D FITS image containing
#   NaN's and then a simple version of rfits
#   (can't use rfits as does conversion!)
$a->inplace->setnantobad;
is( PDL::Core::string( $a->clump(-1) ), 
    "[0 BAD 2 3 0 BAD 2 3 0 BAD]", "inplace setnantobad()" );

# test r/wfits
use PDL::IO::FITS;

$a = sequence(10)->setbadat(0);
print "Writing to fits: $a  type = (", $a->get_datatype, ")\n";
$a->wfits($fname);
$b = rfits($fname);
print "Read from fits:  $b  type = (", $b->get_datatype, ")\n";

ok( $b->slice('0:0')->isbad, "rfits/wfits propogated bad flag" );
ok( sum(abs($a-$b)) < 1.0e-5, "  and values" );

# now force to integer
$a->wfits($fname,16);
$b = rfits($fname);
print "BITPIX 16: datatype == ", $b->get_datatype, " badvalue == ", $b->badvalue(), "\n";
ok( $b->slice('0:0')->isbad, "wfits coerced bad flag with integer datatype" );
ok( sum(abs(convert($a,short)-$b)) < 1.0e-5, "  and the values" );

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

    $a = sequence(4);
    $a->badvalue(3);
    $a->badflag(1);
    $b = $a->slice('2:3');
    is( $b->badvalue, 3, "can propogate per-piddle bad value");
    is( $b->sum, 2, "and the propogated value is recognised as bad");

    $a = sequence(4);
    is ($a->badvalue, double->orig_badvalue, "no long-term affects of per-piddle changes [1]");

}

SKIP: {
    skip ("Skipped: test not valid if per-piddle bad values are used", 1)
      if $perpdl;

    $a = double(4);
    double->badvalue(3);
    is($a->badvalue, double->badvalue, "no long-term affects of per-piddle changes [2]");
    double->badvalue(double->orig_badvalue);

}

# At the moment we do not allow per-piddle bad values
# and the use of NaN's.
#TODO: {
#    local $TODO = "Need to work out whan NaN and per-piddle bad values means";
#    is (0, 1);
#}

# end
