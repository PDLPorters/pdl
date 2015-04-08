# -*-perl-*-

use strict;
use Test::More;

BEGIN {
    plan tests => 19;
}

use PDL::LiteF;

sub tapprox {
    my($a,$b) = @_;
    my $d = max( abs($a-$b) );
    $d < 1.0e-6;
}

# test rvals
#  synonym for centre/center
#  squared option
#  (x|y|z)(lin|log)vals, axisvals

my $x0 = pdl( [ 2, 1, 2 ], [ 1, 0, 1 ], [ 2, 1, 2 ] );

my $a1 = rvals(3,3);
#print "\na1: $a1\n";
ok( tapprox( $x0->sqrt, $a1 ), "centered rvals" ); # 1

my $a2 = rvals(3,3,{squared=>1});
#print "\na2: $a2\n";
ok( tapprox( $x0, $a2 ), "centered rvals squared" ); # 2

my $x1 = pdl( [ 8, 5, 4 ], [ 5, 2, 1 ], [ 4, 1, 0 ] );

my $a3 = rvals(3,3,{centre=>[2,2]});
#print "\na3: $a3\n";
ok( tapprox( $x1->sqrt, $a3 ), "non-centered rvals" ); # 3

my $a4 = rvals(3,3,{center=>[2,2]});
#print "\na4: $a4\n";
ok( tapprox( $x1->sqrt, $a4 ), "centre/center synonyms" ); # 4

ok( tapprox( $x1->sqrt, rvals(3,3,{ceNteR=>[2,2]}) ), "ceNteR option capitalization" ); # 5

ok( tapprox( $x1, rvals(3,3,{center=>[2,2],squared=>1}) ), "both center and squared options" ); # 6

# test (x|y|z)(lin|log)vals: shape and values
{
my $a=zeroes(101,51,26);
my $x = $a->xlinvals(0.5,1.5);
my $y = $a->ylinvals(-2,-1);
my $z = $a->zlinvals(-3,2);
ok(all($a->shape==$x->shape), "xlinvals shape"); #7
ok(all($a->shape==$y->shape), "ylinvals shape"); #8
ok(all($a->shape==$z->shape), "zlinvals shape"); #9
ok(tapprox($x->uniqvec->flat,pdl(50..150)/100),"xlinvals values"); #10
ok(tapprox($y->mv(1,0)->uniqvec->flat,pdl(-100..-50)/50),"ylinvals values"); #11
ok(tapprox($z->mv(2,0)->uniqvec->flat,pdl(0..25)/5-3),"zlinvals values"); #12
}

{
my $a = zeroes(11,6,8);
my $xl = $a->xlogvals(1e2,1e12);
my $yl = $a->ylogvals(1e-3,1e2);
my $zl = $a->zlogvals(1e-10,1e-3);
ok(all($a->shape==$xl->shape),"xlogvals shape"); #13
ok(all($a->shape==$yl->shape),"ylogvals shape"); #14
ok(all($a->shape==$zl->shape),"zlogvals shape"); #15
ok(tapprox($xl->uniqvec->flat->log10,pdl(2..12)),"xlogvals values"); #16
ok(tapprox($yl->mv(1,0)->uniqvec->flat->log10,pdl(-3..2)),"ylogvals values"); #17
ok(tapprox($zl->mv(2,0)->uniqvec->flat->log10,pdl(-10..-3)),"zlogvals values");#18
}
#test axisvals
my $z = axisvals(zeroes(3,4,5,6),3);
ok(all($z==pdl(0..5)->dummy(0,5)->dummy(0,4)->dummy(0,3)),"4-dimensional axisvals");#19
