# -*-perl-*-
#
# test bad value handling in PDL
# - as it's a compile-time option we
#   skip unless $PDL::Config{WITH_BADVAL}
#

use strict;
use Test;

BEGIN { 
    use PDL::Config;
    if ( $PDL::Config{WITH_BADVAL} ) {
	plan tests => 48;
    } else {
	plan tests => 1;
	skip(1,1,1);
	exit;
    }
} 

use PDL::LiteF;
$| = 1;

sub approx {
    my ( $a, $b ) = @_;
    my $d = abs( $a - $b );
    print "diff = [$d]\n";
    return $d <= 0.0001;
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
ok( $a->badflag(), 1 ); # 4

$c = $a + $b;
ok( $c->badflag(), 1 ); # 5
ok( $c->sum(), 21 );    # 6

$a->badflag(0);
$b->badflag(1);
$c = $a + $b;
ok( $c->badflag(), 1 ); # 7

# how about copies/vaffines/whatever
$a = rvals( long, 7, 7, {Centre=>[2,2]} );
$b = $a;
ok( $b->badflag, 0 );   # 8

$a->badflag(1);
$b = $a;
ok( $b->badflag, 1 );   # 9

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
ok( $c->badflag, 1 );   # 11

# can we change bad values
ok( byte->badvalue, byte->orig_badvalue ); # 12
byte->badvalue(23);
ok( byte->badvalue, 23 ); # 13
byte->badvalue( byte->orig_badvalue );

$a = byte(1,2,3);
$b = byte(1,byte->badvalue,3);
$a->badflag(1);
$b->badflag(1);

# does string work?
ok( PDL::Core::string($b), "[1 BAD 3]" );  # 14

# does addition work
$c = $a + $b;
ok( sum($c), 8 ); # 15

# does conversion of bad types work
$c = float($b);
ok( $c->badflag, 1 );        # 16
ok( PDL::Core::string($c), "[1 BAD 3]" );  # 17
ok( sum($c), 4 ); # 18

$a = byte(1,2,byte->badvalue,byte->badvalue,5,6,byte->badvalue,8,9);
$a->badflag(1);

ok( PDL::Core::string($a->isbad),  "[0 0 1 1 0 0 1 0 0]" ); # 19
ok( PDL::Core::string($a->isgood), "[1 1 0 0 1 1 0 1 1]" ); # 20

ok( $a->nbad, 3 );           # 21
ok( $a->ngood, 6 );          # 22

$a = byte( [255,255], [0,255], [0,0] );
$a->badflag(1);
ok( PDL::Core::string($a->nbadover),  "[2 1 0]" );  # 23
ok( PDL::Core::string($a->ngoodover), "[0 1 2]" );  # 24

# check dataflow (or vaffine or whatever it's called)
$a = byte( [1,2,byte->badvalue,4,5], [byte->badvalue,0,1,2,byte->badvalue] );
$a->badflag(1);
$b = $a->slice(',(1)');
ok( sum($b), 3 );            # 25
$b++;
ok( PDL::Core::string($a), "\n[\n [  1   2 BAD   4   5]\n [BAD   1   2   3 BAD]\n]\n" ); # 26 

$a = byte->badvalue * ones(byte,3,2);
ok( $a->get_datatype, 0 );                           # 27
$a->badflag(1);
ok( PDL::Core::string( zcover($a) ), "[BAD BAD]" );  # 28
$a->set(1,1,1); $a->set(2,1,1);
ok( PDL::Core::string( zcover($a) ), "[BAD 0]" );  # 29

$a = byte(1,2,255,4,5);
ok( $a->median, 4 );  # 30
$a->badflag(1);
ok( $a->median, 3 );  # 31

$a = random(20);
$a->badflag(1);
ok( $a->check_badstatus, 0 );            # 32

$i = "Type: %T Dim: %-15D State: %5S  Dataflow: %F";

$a = ushort($a);
$b = $a->badvalue_as_pdl;
print "b = <$b>  info = ", $b->info($i), "\n";
ok( $b->badflag && $b->getndims == 0 && $b->get_datatype == 2, 1 );  # 33

$b = long->badvalue_as_pdl;
print "b = <$b>  info = ", $b->info($i), "\n";
ok( $b->badflag && $b->getndims == 0 && $b->get_datatype == 3, 1 );  # 34

$b = badvalue_as_pdl(0);
print "b = <$b>  info = ", $b->info($i), "\n";
ok( $b->badflag && $b->getndims == 0 && $b->get_datatype == 0, 1 );  # 35

# check out stats, since it uses several routines
# and setbadif
$a = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$b = $a->setbadif( $a < 20 ); 
my @s = $b->stats();                     
ok( approx( $s[0], 61.9375 ), 1 );       # 36
ok( approx( $s[1], 26.7312 ), 1 );       # 37
ok( $s[2], 66.5 );                       # 38
ok( $s[3], 22 );                         # 39
ok( $s[4], 98 );                         # 40

# test some of the qsort functions
my $ix = qsorti( $b );
ok( PDL::Core::string( $b->index($ix) ), 
    "[22 25 32 32 41 42 47 59 74 76 76 79 96 96 96 98 BAD BAD BAD BAD]" 
    );  # 41

# check comparison/bit operators in ops.pd

$a = pdl( 2, 4, double->badvalue );
$a->badflag(1);
$b = abs( $a - pdl(2.001,3.9999,234e23) ) > 0.01;
ok( PDL::Core::string( $b ), "[0 0 BAD]" );  # 42

$b = byte(1,2,255,4);
$b->badflag(1);
ok( PDL::Core::string( $b << 2 ), "[4 8 BAD 16]" );  # 43

# quick look at math.pd
use PDL::Math;

$a = pdl(0.5,double->badvalue,0);
$a->badflag(1);
$b = bessj0($a);
ok( PDL::Core::string( isbad($b) ), "[0 1 0]" );   # 44

$a = pdl(double->badvalue,0.8);
$a->badflag(1);
$b = bessjn($a,3);  # thread over n()
ok( PDL::Core::string( isbad($b) ), "[1 0]" );  # 45
ok( abs($b->at(1)-0.010) < 0.001, 1 );      # 46

$a = pdl( 0.01, 0.0 );
$a->badflag(1);
ok( all( abs(erfi($a)-pdl(0.00886,0)) < 0.001 ), 1 );  # 47

# I haven't changed rotate, but it should work anyway
$a = byte( 0, 1, 255, 4, 5 );
$a->badflag(1);
ok( PDL::Core::string( $a->rotate(2) ), "[4 5 0 1 BAD]" ); # 48

# check indadd
