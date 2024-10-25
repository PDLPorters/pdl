use strict;
use warnings;
use Test::More;
use PDL::LiteF;

sub tapprox {
    my($x,$y) = @_;
    my $d = max( abs($x-$y) );
    $d < 1.0e-6;
}

my $x0 = pdl( [ 2, 1, 2 ], [ 1, 0, 1 ], [ 2, 1, 2 ] );

my $a1 = rvals(3,3);
ok( tapprox( $x0->sqrt, $a1 ), "centered rvals" ) or diag $a1;

my $a2 = rvals(3,3,{squared=>1});
ok( tapprox( $x0, $a2 ), "centered rvals squared" ) or diag $a2;

my $x1 = pdl( [ 8, 5, 4 ], [ 5, 2, 1 ], [ 4, 1, 0 ] );

my $a3 = rvals(3,3,{centre=>[2,2]});
ok( tapprox( $x1->sqrt, $a3 ), "non-centered rvals" ) or diag $a3;

my $a4 = rvals(3,3,{center=>[2,2]});
ok( tapprox( $x1->sqrt, $a4 ), "centre/center synonyms" ) or diag $a4;

my $a5 = rvals(3,3,{ceNteR=>[2,2]});
ok( tapprox( $x1->sqrt, $a5 ), "ceNteR option capitalization" ) or diag $a5;

my $a6 = rvals(3,3,{center=>[2,2],squared=>1});
ok( tapprox( $x1, $a6 ), "both center and squared options" ) or diag $a6;

# test (x|y|z)(lin|log)vals: shape and values
{
my $a1=zeroes(101,51,26);
my $x = $a1->xlinvals(0.5,1.5);
my $y = $a1->ylinvals(-2,-1);
my $z = $a1->zlinvals(-3,2);
ok(all($a1->shape==$x->shape), "xlinvals shape"); #7
ok(all($a1->shape==$y->shape), "ylinvals shape"); #8
ok(all($x->shape==$z->shape), "zlinvals shape"); #9
ok(tapprox($x->uniqvec->flat,pdl(50..150)/100),"xlinvals values"); #10
ok(tapprox($y->mv(1,0)->uniqvec->flat,pdl(-100..-50)/50),"ylinvals values"); #11
ok(tapprox($z->mv(2,0)->uniqvec->flat,pdl(0..25)/5-3),"zlinvals values"); #12
$a1->inplace->xvals;
my $got = $a1->slice('(10),(0),(0)');
ok tapprox($got, 10), 'inplace xvals works' or diag "got:$got";
my $lin1 = eval { zeroes(1)->xlinvals(5,10) };
is $@, '', 'can have length-one *linvals';
ok tapprox($lin1, 5), 'length-one *linvals gives starting point';
eval { zeroes(0)->xlinvals(5,10) };
like $@, qr/at least/, 'cannot have length-zero dim *linvals';
my $byte_xvals = ones( byte, 300 )->xvals;
is $byte_xvals->type, 'double', 'byte->xvals type double';
is $byte_xvals->at(280), 280,'non-overflow xvals from byte ndarray';
is xvals(short, 2)->type, 'short', 'xvals respects specified type';
}

{
my $x = zeroes(11,6,8);
my $xl = $x->xlogvals(1e2,1e12);
my $yl = $x->ylogvals(1e-3,1e2);
my $zl = $x->zlogvals(1e-10,1e-3);
ok(all($x->shape==$xl->shape),"xlogvals shape"); #13
ok(all($x->shape==$yl->shape),"ylogvals shape"); #14
ok(all($x->shape==$zl->shape),"zlogvals shape"); #15
ok(tapprox($xl->uniqvec->flat->log10,pdl(2..12)),"xlogvals values"); #16
ok(tapprox($yl->mv(1,0)->uniqvec->flat->log10,pdl(-3..2)),"ylogvals values"); #17
ok(tapprox($zl->mv(2,0)->uniqvec->flat->log10,pdl(-10..-3)),"zlogvals values");#18
my $log1 = eval { zeroes(1)->xlogvals(5,10) };
is $@, '', 'can have length-one *logvals';
ok tapprox($log1, 5), 'length-one *logvals gives starting point';
eval { zeroes(0)->xlogvals(5,10) };
like $@, qr/at least/, 'cannot have length-zero *logvals';
}
#test axisvals
my $z = axisvals(zeroes(3,4,5,6),3);
ok(all($z==pdl(0..5)->dummy(0,5)->dummy(0,4)->dummy(0,3)),"4-dimensional axisvals");#19

{
my $x = pdl [15.4,15.8,16.01,16.9,16.1,15.2,15.4,16.2,15.4,16.2,16.4];
eval { hist ($x,15,15,0.1) }; # shouldn't segfault!
isnt $@, '', 'error thrown';
my ($hx,$h) = hist ($x,15,17,0.1);
ok( tapprox($hx, pdl(qw/15.05   15.15 15.25   15.35   15.45   15.55   15.65
   15.75   15.85   15.95   16.05   16.15 16.25   16.35   16.45   16.55   16.65
   16.75   16.85   16.95/)), "bin centers");
ok( tapprox($h, pdl(qw/0 1 0 0 3 0 0 0 1 0 1 3 0 1 0 0 0 0 1 0/)), "hist vals");
}

{
my $x  = pdl( qw{ 13 10 13 10 9 13 9 12 11 10 10 13 7 6 8 10 11 7 12 9
	       11 11 12 6 12 7} );
my $wt = pdl( qw{ -7.4733817 -3.0945993 -1.7320649 -0.92823577 -0.34618392
	       -1.3326057 -1.3267382 -0.032047153 0.067103333 -0.11446796
	       -0.72841944 0.95928255  1.4888114 0.17143622 0.14107419
	       -1.6368404    0.72917 -2.0766962 -0.66708236 -0.52959271
	       1.1551274   0.079184  1.4068289 0.038689811 0.87947996
	       -0.88373274  } );
my ( $hx, $h ) = whist ($x, $wt, 0, 20, 1 );
ok( tapprox($hx,
	 pdl(qw{ 0.5 1.5 2.5 3.5 4.5 5.5 6.5 7.5 8.5 9.5 10.5
		 11.5 12.5 13.5 14.5 15.5 16.5 17.5 18.5 19.5 }) ), "weighted bin centers");
ok( tapprox($h,
	 pdl(qw{ 0 0 0 0 0 0 0.21012603 -1.4716175 0.14107419 -2.2025149
		 -6.5025629  2.0305847  1.5871794 -9.5787698 0 0 0 0 0 0 }) ), "weighted hist vals");
}

my $a0 = zeroes(3,2);

$a1 = xvals $a0;

is($a1->at(0,0), 0, "xvals 0,0 == 0");
is($a1->at(1,0), 1, "xvals 1,0 == 1");
is($a1->at(2,0), 2, "xvals 2,0 == 2");
is($a1->at(1,1), 1, "xvals 1,1 == 1");

# sequence as instance method
my $seq_src = pdl(indx, [9,8,7]);
my $seq_dst = $seq_src->sequence;
is $seq_dst->type, $seq_src->type, 'sequence as instance-method should maintain type';
is_deeply [$seq_dst->dims], [$seq_src->dims], "sequence as instance-method should maintain dims";
is_deeply [$seq_dst->list], [0..($seq_src->nelem-1)], "sequence as instance-method should enumerate all elements";

done_testing;
