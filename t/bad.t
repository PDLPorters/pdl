# -*-perl-*-
#
# test bad value handling in PDL
# - as it's a compile-time option we
#   skip unless $PDL::Config{WITH_BADVAL}
#

use strict;
use Test;

my $fname = 'delme.fits';

END {
    unlink $fname if -e $fname;
}

use PDL::LiteF;
$| = 1;

sub tapprox {
    my ( $a, $b ) = @_;
    my $d = abs( $a - $b );
    print "diff = [$d]\n";
    return $d <= 0.0001;
}

use PDL::Config;
if ( $PDL::Config{WITH_BADVAL} ) {
    plan tests => 70;
} else {
    # reduced testing
    plan tests => 10;

    my $a = pdl(1,2,3);
    ok( $a->badflag(), 0 ); # 1
    
    my $b = pdl(4,5,6);
    my $c = $a + $b;
    ok( $c->badflag(), 0 ); # 2
    ok( $c->sum(), 21 );    # 3
    
    # can not set the bad flag
    $a->badflag(1);
    ok( $a->badflag() == 0 );    

    # and piddles do not have a bad value
    ok( ! defined $a->badvalue );

    # can not change a piddle to include bad values
    ok( all( ($a->setbadif( $a == 2 ) - pdl(1,2,3)) == 0 ) );

    $a = ones(3,2,4);
    $b = zeroes(2,4);
    $c = ones(2,4) * 3;
    ok( $a->nbad  == 0 );
    ok( $a->ngood == 24 );
    ok( all( ($a->nbadover  - $b) == 0 ) );
    ok( all( ($a->ngoodover - $c) == 0 ) );

    exit;
}

# check default behaviour (ie no bad data)
# - probably overkill
#
my $a = pdl(1,2,3);
ok( $a->badflag(), 0 ); # 1

my $b = pdl(4,5,6);
my $c = $a + $b;
ok( $c->badflag(), 0 ); # 2
ok( $c->sum(), 21 );    # 3

# is the flag propogated?
$a->badflag(1);
ok( $a->badflag() ); # 4

$c = $a + $b;
ok( $c->badflag() ); # 5
ok( $c->sum(), 21 );    # 6

$a->badflag(0);
$b->badflag(1);
$c = $a + $b;
ok( $c->badflag() ); # 7

# how about copies/vaffines/whatever
$a = rvals( long, 7, 7, {Centre=>[2,2]} );
$b = $a;
ok( $b->badflag, 0 );   # 8

$a->badflag(1);
$b = $a;
ok( $b->badflag );   # 9

$a->badflag(0);
$b = $a->slice('2:5,3:4');
$c = $b->slice('0:1,(0)'); 
ok( $b->badflag, 0 );   # 10

$a->badflag(1);

my $i = "Type: %T Dim: %-15D State: %5S  Dataflow: %F";
print "Info: a = ", $a->info($i), "\n";
print "Info: b = ", $b->info($i), "\n";
print "Info: c = ", $b->info($i), "\n";

# let's check that it gets through to a child of a child
ok( $c->badflag );   # 11

# can we change bad values
ok( byte->badvalue, byte->orig_badvalue ); # 12
byte->badvalue(23);
ok( byte->badvalue, 23 );                  # 13
byte->badvalue( byte->orig_badvalue );

# check setbadat()
$a = pdl(1,2,3,4,5);
$a->setbadat(2);
ok( PDL::Core::string($a), "[1 2 BAD 4 5]" );   # 14

# now check that badvalue() changes the piddle
# (only for integer types)
$a = convert($a,ushort);
my $badval = $a->badvalue;
$a->badvalue(44);
ok( PDL::Core::string($a), "[1 2 BAD 4 5]" );   # 15
$a->badflag(0);
ok( PDL::Core::string($a), "[1 2 44 4 5]" );    # 16

# restore the bad value
$a->badvalue($badval);

$a = byte(1,2,3);
$b = byte(1,byte->badvalue,3);
$a->badflag(1);
$b->badflag(1);

# does string work?
ok( PDL::Core::string($b), "[1 BAD 3]" );  # 

# does addition work
$c = $a + $b;
ok( sum($c), 8 );                          # 

# does conversion of bad types work
$c = float($b);
ok( $c->badflag );                         # 
ok( PDL::Core::string($c), "[1 BAD 3]" );  # 20
ok( sum($c), 4 );                          # 

$a = byte(1,2,byte->badvalue,byte->badvalue,5,6,byte->badvalue,8,9);
$a->badflag(1);

ok( PDL::Core::string($a->isbad),  "[0 0 1 1 0 0 1 0 0]" ); # 
ok( PDL::Core::string($a->isgood), "[1 1 0 0 1 1 0 1 1]" ); # 

ok( $a->nbad, 3 );           # 
ok( $a->ngood, 6 );          # 25

$a = byte( [255,255], [0,255], [0,0] );
$a->badflag(1);
ok( PDL::Core::string($a->nbadover),  "[2 1 0]" );  # 
ok( PDL::Core::string($a->ngoodover), "[0 1 2]" );  # 

# check dataflow (or vaffine or whatever it's called)
$a = byte( [1,2,byte->badvalue,4,5], [byte->badvalue,0,1,2,byte->badvalue] );
$a->badflag(1);
$b = $a->slice(',(1)');
ok( sum($b), 3 );                      # 
$b++;
ok( PDL::Core::string($a), "\n[\n [  1   2 BAD   4   5]\n [BAD   1   2   3 BAD]\n]\n" ); # 

$a = byte->badvalue * ones(byte,3,2);
ok( $a->get_datatype, 0 );                           # 30 
$a->badflag(1);
ok( PDL::Core::string( zcover($a) ), "[BAD BAD]" );  # 
$a->set(1,1,1); $a->set(2,1,1);
ok( PDL::Core::string( zcover($a) ), "[BAD 0]" );  # 

$a = byte(1,2,255,4,5);
ok( $a->median, 4 );             # 
$a->badflag(1);
ok( $a->median, 3 );             # 

$a = random(20);
$a->badflag(1);
ok( $a->check_badflag, 0 );            # 35

$i = "Type: %T Dim: %-15D State: %5S  Dataflow: %F";

# check out stats, since it uses several routines
# and setbadif
$a = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$b = $a->setbadif( $a < 20 ); 
my @s = $b->stats();                     
ok( tapprox( $s[0], 61.9375 ) );       # 
ok( tapprox( $s[1], 26.7312 ) );       # 
ok( $s[2], 66.5 );                       # 
ok( $s[3], 22 );                         # 
ok( $s[4], 98 );                         # 40

# how about setbadtoval (was replacebad)
$a = $b->setbadtoval(20) - pdl(qw(42 47 98 20 22 96 74 41 79 76 96 20 32 76 25 59 20 96 32 20));
ok( all($a == 0) );                   # 

# and inplace?
$a = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$b = $a->setbadif( $a < 20 ); 
$b->inplace->setbadtoval(20);
$a = $b - pdl(qw(42 47 98 20 22 96 74 41 79 76 96 20 32 76 25 59 20 96 32 20));
ok( all($a == 0) );                   # 

# ditto for copybad
$a = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$b = $a->setbadif( $a < 20 ); 
$c = copybad( $a, $b );
ok( PDL::Core::string( $c->isbad ), 
    "[0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1]" ); # 

$a = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$b = $a->setbadif( $a < 20 ); 
$a->inplace->copybad( $b );
ok( PDL::Core::string( $a->isbad ), 
    "[0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 1]" ); #

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
ok( $b->badflag, 0 );                  # 45

$a = sequence( byte, 2, 3 );
$b = $a->slice("(1),:");
my $mask = sequence( byte, 2, 3 );
$mask = $mask->setbadif( ($mask % 3) == 2 );
print "a,b == ", $a->badflag, ",", $b->badflag, "\n";
$a->inplace->copybad( $mask );
print "a,b == ", $a->badflag, ",", $b->badflag, "\n";
print "$a $b\n";
ok( $b->badflag, 1 );                  # 

# test some of the qsort functions
$a = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$b = $a->setbadif( $a < 20 ); 
my $ix = qsorti( $b );
ok( PDL::Core::string( $b->index($ix) ), 
    "[22 25 32 32 41 42 47 59 74 76 76 79 96 96 96 98 BAD BAD BAD BAD]" 
    );                                   # 

# check comparison/bit operators in ops.pd

$a = pdl( 2, 4, double->badvalue );
$a->badflag(1);
$b = abs( $a - pdl(2.001,3.9999,234e23) ) > 0.01;
ok( PDL::Core::string( $b ), "[0 0 BAD]" );  # 

$b = byte(1,2,byte->badvalue,4);
$b->badflag(1);
ok( PDL::Core::string( $b << 2 ), "[4 8 BAD 16]" );  # 

# quick look at math.pd
use PDL::Math;

$a = pdl(0.5,double->badvalue,0);
$a->badflag(1);
$b = bessj0($a);
ok( PDL::Core::string( isbad($b) ), "[0 1 0]" );   # 50

$a = pdl(double->badvalue,0.8);
$a->badflag(1);
$b = bessjn($a,3);  # thread over n()
ok( PDL::Core::string( isbad($b) ), "[1 0]" );  # 
ok( abs($b->at(1)-0.010) < 0.001 );             # 

$a = pdl( 0.01, 0.0 );
$a->badflag(1);
ok( all( abs(erfi($a)-pdl(0.00886,0)) < 0.001 ) );  # 

# I haven't changed rotate, but it should work anyway
$a = byte( 0, 1, 2, 4, 5 );
$a->setbadat(2);
ok( PDL::Core::string( $a->rotate(2) ), "[4 5 0 1 BAD]" ); # 

# check indadd, norm

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

ok( int(at(sum($c-$ans))), 0 ); # 55

$a = zeroes(5,5);
$a->badflag(1);
my $t = $a->slice("1:3,1:3");
$t .= ones(3,3);
$a->setbadat(2,2);

$b = sequence(3,3);
$ans = pdl ( [0,0,0,0,0],[0,0,2,0,0],[0,1,5,2,0],[0,0,4,0,0],[0,0,0,0,0]);
ok( int(at(sum(med2d($a,$b)-$ans))), 0 );  # 

# propogation of badflag using inplace ops (ops.pd)

# test biop fns
$a = sequence(3,3);
$c = $a->slice(',(1)');
$b = $a->setbadif( $a % 2 );
$a->inplace->plus($b,0);
print $a;
print "$c\n";
ok( PDL::Core::string($c), "[BAD 8 BAD]" );  #

# test bifunc fns
$a = sequence(3,3);
$c = $a->slice(',(1)');
$b = $a->setbadif( $a % 3 != 0 );
$a->inplace->power($b,0);
print $a;
print "$c\n";
ok( PDL::Core::string($c), "[27 BAD BAD]" );  #

# test histogram (using hist)
$a = pdl( qw/1 2 3 4 5 4 3 2 2 1/ );
$a->setbadat(1);
$b = hist $a, 0, 6, 1;
print "values:    $a\n";
print "histogram: $b\n";
ok( PDL::Core::string($b), "[0 2 2 2 2 1]" ); # 

#$b = $a->isfinite;
#print "isfinite(A): datatype = [",$b->get_datatype,"]\n";

$a->inplace->isfinite;
#print "A: datatype = [",$a->get_datatype,"]\n";
ok( PDL::Core::string($a), "[1 0 1 1 1 1 1 1 1 1]" ); # 60
#print "A: datatype = [",$a->get_datatype,"]\n";

# histogram2d
$a = long(1,1,1,2,2);
$b = long(2,1,1,1,1);
$b->setbadat(0);
my @c = ( 1,0,3 );
$c = histogram2d($a,$b,@c,@c);
ok( PDL::Core::string($c->clump(-1)), 
    "[0 0 0 0 2 2 0 0 0]" );             # 

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
ok( PDL::Core::string($a), "[0 1 0 3 4]" );  #

# setvaltobad
$a = sequence(10) % 4;
$a->inplace->setvaltobad( 1 );
ok( PDL::Core::string( $a->clump(-1) ), 
    "[0 BAD 2 3 0 BAD 2 3 0 BAD]" );   #

# simple test for setnantobad
# - could have a 1D FITS image containing
#   NaN's and then a simple version of rfits
#   (can't use rfits as does conversion!)
$a->inplace->setnantobad;
ok( PDL::Core::string( $a->clump(-1) ), 
    "[0 BAD 2 3 0 BAD 2 3 0 BAD]" );   #

# test r/wfits
use PDL::IO::Misc;
$a = sequence(10)->setbadat(0);
print "Writing to fits: $a  type = (", $a->get_datatype, ")\n";
$a->wfits($fname);
$b = rfits($fname);
print "Read from fits:  $b  type = (", $b->get_datatype, ")\n";

ok( $b->slice('0:0')->isbad );      # 65
ok( sum(abs($a-$b)) < 1.0e-5 );      # 

# now force to integer
$a->wfits($fname,16);
$b = rfits($fname);
print "BITPIX 16: datatype == ", $b->get_datatype, " badvalue == ", $b->badvalue(), "\n";
ok( $b->slice('0:0')->isbad );      # 
ok( sum(abs(convert($a,short)-$b)) < 1.0e-5 );      # 

# check that we can change the value used to represent
# missing elements for floating points (earlier tests only did integer types)
# IF we are not using NaN's
#
if ( $PDL::Config{BADVAL_USENAN} || 0 ) {
    # perhaps should check that the value can't be changed?
    skip( "Skipped: test only valid when not using NaN's as bad values", 1, 1 ); #
    skip( "Skipped: test only valid when not using NaN's as bad values", 1, 1 ); # 70
} else {
    ok( float->badvalue, float->orig_badvalue );  #
    ok( float->badvalue(23), 23 );                # 70
    float->badvalue( float->orig_badvalue );
}
