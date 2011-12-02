no warnings qw(misc);
use Test::More tests => 23;

use PDL::LiteF;
use PDL::Tests;
use PDL::Types;
use PDL::Dbg;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

# Is there any good reason we don't use PDL's approx function?
sub tapprox {
    my($a,$b,$c,$d) = @_;
    $c = abs($a-$b);
    $d = max($c);
    return $d < 0.01;
}

$a = xvals(zeroes(byte, 2, 4));

# $P() affine tests
test_foop($a,($b=null));
ok( tapprox($a,$b) )
  or diag $b;

test_foop($a->xchg(0,1),($b=null));
ok( tapprox($a->xchg(0,1),$b) )
  or diag $b;

$vaff = $a->dummy(2,3)->xchg(1,2);
test_foop($vaff,($b=null));
ok( tapprox($vaff,$b) )
  or diag ($vaff, $vaff->dump);


# now in primitive.t
# double qualifier
#$a = ones(byte,3000);
#test_dsumover($a,($b=null));
#ok( $b->get_datatype, $PDL_D );
#ok( $b->at, 3000 );

# float qualifier
$a = ones(byte,3000);
test_fsumover($a,($b=null));
is( $b->get_datatype, $PDL_F );
is( $b->at, 3000 );

# int+ qualifier
for (byte,short,ushort,long,float,double) {
  $a = ones($_,3000);
  test_nsumover($a,($b=null));
  is( $b->get_datatype, (($PDL_L > $_->[0]) ? $PDL_L : $_->[0]) );
  is( $b->at, 3000 );
}

test_setdim(($a=null),10);
is( join(',',$a->dims), "10" );
ok( tapprox($a,sequence(10)) );

# this used to segv under solaris according to Karl
{ local $=0; # To suppress warnings of use of uninitialized value.
  $ny=7;
  $a = double xvals zeroes (20,$ny);
  test_fooseg $a, $b=null;

  ok( 1 );  # if we get here at all that is alright
  ok( tapprox($a,$b) )
    or diag($a, "\n", $b);
}


# test the bug alluded to in the comments in
# pdl_changed (pdlapi.c)
# used to segfault
$xx=ones(float,3,4);
$sl1 = $xx->slice('(0)');
$sl11 = $sl1->slice('');
$sl2 = $xx->slice('(1)');
$sl22 = $sl2->slice('');

test_fooflow2 $sl11, $sl22;

ok(all $xx->slice('(0)') == 599);
ok(all $xx->slice('(1)') == 699);
