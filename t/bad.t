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
#	plan tests => 24, todo => [11,17];
	plan tests => 36;
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
ok( $a->baddata(), 0 ); # 1

my $b = pdl(4,5,6);
my $c = $a + $b;
ok( $c->baddata(), 0 ); # 2
ok( $c->sum(), 21 );    # 3

# is the flag propogated?
$a->baddata(1);
ok( $a->baddata(), 1 ); # 4

$c = $a + $b;
ok( $c->baddata(), 1 ); # 5
ok( $c->sum(), 21 );    # 6

$a->baddata(0);
$b->baddata(1);
$c = $a + $b;
ok( $c->baddata(), 1 ); # 7

# how about copies/vaffines/whatever
$a = rvals( long, 7, 7, {Centre=>[2,2]} );
$b = $a;
ok( $b->baddata, 0 );   # 8

$a->baddata(1);
$b = $a;
ok( $b->baddata, 1 );   # 9

$a->baddata(0);
$b = $a->slice('2:5,3:4');
$c = $b->slice('0:1,(0)'); 
ok( $b->baddata, 0 );   # 10

$a->baddata(1);

my $i = "Type: %T Dim: %-15D State: %5S  Dataflow: %F";
print "Info: a = ", $a->info($i), "\n";
print "Info: b = ", $b->info($i), "\n";
print "Info: c = ", $b->info($i), "\n";

# let's check that it gets through to a child of a child
ok( $c->baddata, 1 );   # 11

# can we change bad values
ok( byte->badvalue, byte->orig_badvalue ); # 12
byte->badvalue(23);
ok( byte->badvalue, 23 ); # 13
byte->badvalue( byte->orig_badvalue );

$a = byte(1,2,3);
$b = byte(1,byte->badvalue,3);
$a->baddata(1);
$b->baddata(1);

# does string work?
ok( PDL::Core::string($b), "[1 BAD 3]" );  # 14

# does addition work
$c = $a + $b;
ok( sum($c), 8 ); # 15

# does conversion of bad types work
$c = float($b);
ok( $c->baddata, 1 );        # 16
ok( PDL::Core::string($c), "[1 BAD 3]" );  # 17
ok( sum($c), 4 ); # 18

$a = byte(1,2,byte->badvalue,byte->badvalue,5,6,byte->badvalue,8,9);
$a->baddata(1);

ok( PDL::Core::string($a->isbad),  "[0 0 1 1 0 0 1 0 0]" ); # 19
ok( PDL::Core::string($a->isgood), "[1 1 0 0 1 1 0 1 1]" ); # 20

ok( $a->nbad, 3 );           # 21
ok( $a->ngood, 6 );          # 22

$a = byte( [255,255], [0,255], [0,0] );
$a->baddata(1);
ok( PDL::Core::string($a->nbadover),  "[2 1 0]" );  # 23
ok( PDL::Core::string($a->ngoodover), "[0 1 2]" );  # 24

# check dataflow (or vaffine or whatever it's called)
$a = byte( [1,2,byte->badvalue,4,5], [byte->badvalue,0,1,2,byte->badvalue] );
$a->baddata(1);
$b = $a->slice(',(1)');
ok( sum($b), 3 );            # 25
$b++;
ok( PDL::Core::string($a), "\n[\n [  1   2 BAD   4   5]\n [BAD   1   2   3 BAD]\n]\n" ); # 26 

$a = byte->badvalue * ones(byte,3,2);
ok( $a->get_datatype, 0 );                           # 27
$a->baddata(1);
ok( PDL::Core::string( zcover($a) ), "[BAD BAD]" );  # 28
$a->set(1,1,1); $a->set(2,1,1);
ok( PDL::Core::string( zcover($a) ), "[BAD 1]" );  # 29

$a = byte(1,2,255,4,5);
ok( $a->median, 4 );  # 30
$a->baddata(1);
ok( $a->median, 3 );  # 31

# check out stats, since it uses several routines
$a = pdl( qw(42 47 98 13 22 96 74 41 79 76 96 3 32 76 25 59 5 96 32 6) );
$b = ($a < 20) * double->badvalue;
$b->baddata(1);
$b += $a;

# note; we can't say
#   $b = ($a < 20) * double->badvalue + $a;
#
# because double->badvalue returns a scalar, so 
# '($a<20)*double->badvalue' is good, hence the
# addition occurs - all the bad values are lost.
# (this wouldn't be a problem if we used NaN's)
#
my @s = $b->stats();                     
ok( approx( $s[0], 61.9375 ), 1 );       # 32
ok( approx( $s[1], 26.7312 ), 1 );       # 33
ok( $s[2], 66.5 );                       # 34
ok( $s[3], 22 );                         # 35
ok( $s[4], 98 );                         # 36
