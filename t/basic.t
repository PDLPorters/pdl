# -*-perl-*-

use strict;
use Test;

BEGIN {
    plan tests => 6;
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
#

my $x0 = pdl( [ 2, 1, 2 ], [ 1, 0, 1 ], [ 2, 1, 2 ] );

my $a1 = rvals(3,3);
print "\na1: $a1\n";
ok( tapprox( $x0->sqrt, $a1 ), 1 ); # 1

my $a2 = rvals(3,3,{squared=>1});
print "\na2: $a2\n";
ok( tapprox( $x0, $a2 ), 1 ); # 2

my $x1 = pdl( [ 8, 5, 4 ], [ 5, 2, 1 ], [ 4, 1, 0 ] );

my $a3 = rvals(3,3,{centre=>[2,2]});
print "\na3: $a3\n";
ok( tapprox( $x1->sqrt, $a3 ), 1 ); # 3

my $a4 = rvals(3,3,{center=>[2,2]});
print "\na4: $a4\n";
ok( tapprox( $x1->sqrt, $a4 ), 1 ); # 4

ok( tapprox( $x1->sqrt, rvals(3,3,{ceNteR=>[2,2]}) ), 1 ); # 5

ok( tapprox( $x1, rvals(3,3,{center=>[2,2],squared=>1}) ), 1 ); # 6

