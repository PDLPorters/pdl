# -*-perl-*-
# Script to test some of the primitive operations for returning the correct values.
#
#  
#  Testing utility functions:

sub tapprox {
        my($a,$b) = @_;
        my $c = abs($a-$b);
        my $d = ref($c) ? max($c) : $c ;  # don't do a make if were are dealing 
					  # with a scalar
        $d < 0.01;
}

use PDL::LiteF;
use Test::More;
use strict;

plan tests => 30;

###### Testing Begins #########

my $im = new PDL [
  [ 1, 2,  3,  3 , 5],
  [ 2,  3,  4,  5,  6],
  [13, 13, 13, 13, 13],
  [ 1,  3,  1,  3,  1],
  [10, 10,  2,  2,  2,]
 ];


my @minMax = $im->minmax;
# print "MinMax = ".join(", ",@minMax)."\n";



ok($minMax[0] == 1, "minmax min" );
ok($minMax[1] == 13, "minmax max" );


ok(($im x $im)->sum == 3429, "matrix multiplication" );


my @statsRes = $im->stats;

ok(tapprox($statsRes[0],5.36), "stats: mean" );
ok(tapprox($statsRes[1],4.554), "stats: prms");
ok(tapprox($statsRes[2],3), "stats: median");
ok(tapprox($statsRes[3],1), "stats: min");
ok(tapprox($statsRes[4],13), "stats: max");
ok(tapprox($statsRes[6],4.462), "stats: rms");

@statsRes = $im->short->stats; # Make sure that stats are promoted to floating-point

ok(tapprox($statsRes[0],5.36), "stats: float mean");
ok(tapprox($statsRes[1],4.554), "stats: float prms");
ok(tapprox($statsRes[2],3), "stats: float median");
ok(tapprox($statsRes[3],1), "stats: float min");
ok(tapprox($statsRes[4],13), "stats: float max");
ok(tapprox($statsRes[6],4.462), "stats: float rms");

# print "StatRes = ".join(", ",@statsRes)."\n";


my $ones = ones(5,5);

@statsRes = $im->stats($ones);

# print "StatRes with moments = ".join(", ",@statsRes)."\n";
ok(tapprox($statsRes[0],5.36), "stats: trivial weights mean" );
ok(tapprox($statsRes[1],4.554), "stats: trivial weights prms" );
ok(tapprox($statsRes[2],3), "stats: trivial weights median" );
ok(tapprox($statsRes[3],1), "stats: trivial weights min" );
ok(tapprox($statsRes[4],13), "stats: trivial weights max" );
ok(tapprox($statsRes[6],4.462), "stats: trivial weights rms");

# which ND test
my $a= PDL->sequence(10,10,3,4);  

# $PDL::whichND_no_warning = 1;
# ($x, $y, $z, $w)=whichND($a == 203);
my ($x, $y, $z, $w) = whichND($a == 203)->mv(0,-1)->dog;  # quiet deprecation warning

ok($a->at($x->list,$y->list,$z->list,$w->list) == 203, "whichND" );
 
$a = pdl(1,2,3,4);
$b = append($a,2);
ok(int(sum($b))==12, "append");



# clip tests
ok(tapprox($im->hclip(5)->sum,83), "hclip" );

ok(tapprox($im->lclip(5)->sum,176), "lclip" );


ok(tapprox($im->clip(5,7)->sum,140), "clip" );

# indadd Test:
$a = pdl( 1,2,3);
my $ind = pdl( 1,4,6);
my $sum = zeroes(10);
indadd($a,$ind, $sum);
ok(tapprox($sum->sum,6), "indadd" );

#one2nd test
$a = zeroes(3,4,5);
my $indicies = pdl(0,1,4,6,23,58,59);
($x,$y,$z)=$a->one2nd($indicies);
ok(all( $x==pdl(0,1,1,0,2,1,2) ), "one2nd x");
ok(all( $y==pdl(0,0,1,2,3,3,3) ), "one2nd y");
ok(all( $z==pdl(0,0,0,0,1,4,4) ), "one2nd z");
