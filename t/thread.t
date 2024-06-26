use strict;
use warnings;
use Test::More;
use PDL::LiteF;

{
  # 2. If we don't want flow, we mustn't have it.
  my $pa = pdl 2,3,4;
  my $pb = $pa + $pa;
  is "$pb", '[4 6 8]';
  $pa->set(0,50);
  is "$pb", '[4 6 8]';
}

{
  # 3. Test what happens when we assign to $pb. (no coredumps allowed)
  my $pa = pdl 2,3,4;
  $pa->flowing;
  my $pb = $pa + $pa;
  is "$pb", '[4 6 8]';
  $pb->set(0,50);
  $pb->sever; # As of 2.064 you must break the dataflow manually
  is "$pb", '[50 6 8]';
  is "$pa", '[2 3 4]';
  $pa->set(0,33);
  is "$pa", '[33 3 4]';
  is "$pb", '[50 6 8]';
}

{
  # 4. Now a basic slice test.
  my $pa = pdl [2,3,4],[5,6,7];
  is("$pa", "\n[\n [2 3 4]\n [5 6 7]\n]\n");
  my $pb = $pa->slice('1:2,:');
  is("$pb", "\n[\n [3 4]\n [6 7]\n]\n");
  $pa->set(1,1,9);
  is("$pa", "\n[\n [2 3 4]\n [5 9 7]\n]\n");
  is("$pb", "\n[\n [3 4]\n [9 7]\n]\n");
  my $pc = $pa->slice('0:1,:');
  is("$pc", "\n[\n [2 3]\n [5 9]\n]\n");
  $pb->set(0,0,8);
  is("$pa", "\n[\n [2 8 4]\n [5 9 7]\n]\n");
  is("$pb", "\n[\n [8 4]\n [9 7]\n]\n");
  is("$pc", "\n[\n [2 8]\n [5 9]\n]\n");
}

# 5. Now, to the hairy stuff of generations and progenitors.

# 7. What about axisvals:
{
  my $pa = zeroes 5,3;
  is("$pa", "\n[\n [0 0 0 0 0]\n [0 0 0 0 0]\n [0 0 0 0 0]\n]\n");
  my $pb = PDL::Core::new_or_inplace($pa);
  my $pc = $pb->transpose;
  axisvalues($pc->inplace);
  is("$pc", "\n[\n [0 1 2]\n [0 1 2]\n [0 1 2]\n [0 1 2]\n [0 1 2]\n]\n");
  is("$pb", "\n[\n [0 0 0 0 0]\n [1 1 1 1 1]\n [2 2 2 2 2]\n]\n");
  is("$pa", "\n[\n [0 0 0 0 0]\n [0 0 0 0 0]\n [0 0 0 0 0]\n]\n");
  $pa = zeroes 5,5;
  $pb = $pa->slice("1:3,1:3");
  $pc = $pb->slice("(1),(1)");
  is($pc->at(), 0);
  $pa .= 1;
  is($pc->at(), 1);
  $pa .= 2;
  is($pc->at(), 2);
}

{
my $pa = pdl [2,3,4],[5,6,7];
$pa->flowing;
my $a2 = pdl 1;
my $pb = $pa + $a2;
is("$pb", "\n[\n [3 4 5]\n [6 7 8]\n]\n", 'pb flowing');
my $pc = $pb * 2; # This should stay the same flowed structure.
is("$pc", "\n[\n [ 6  8 10]\n [12 14 16]\n]\n", 'multiplied');
}

{
# Then, the more difficult ways: explicit broadcasting.
# Dims: 3,3,2
my $pa = pdl [[0,1,2],[3,4,5],[6,7,8]],[[10,11,12],[13,14,15],[16,17,18]];
my $pb = zeroes(3,3);
my $pc = $pb->broadcast(0,1);
is $pc->info, 'PDL: Double D [] T1 [3,3]', 'info right for explicit broadcasting 1 dim';
is $pb->broadcast(0)->info, 'PDL: Double D [3] T1 [3]', 'info right for explicit broadcasting 2 dims';
is zeroes(4,7,2,8)->broadcast(2)->info, 'PDL: Double D [4,7,8] T1 [2]', 'info right for higher-dim explicit broadcasting 1 dims';
is zeroes(4,7,2,8)->broadcast(2,1)->info, 'PDL: Double D [4,8] T1 [2,7]', 'info right for higher-dim explicit broadcasting 2 dims';
is zeroes(4,7,2,8,5,6)->broadcast(2,4)->info, 'PDL: Double D [4,7,8,6] T1 [2,5]', 'info right for higher-dim explicit broadcasting 2 dims';
is zeroes(4,7,2,8,5,6)->broadcast1(2)->broadcast2(3)->info, 'PDL: Double D [4,7,8,6] T1 [2] T2 [5]', 'info right for higher-dim explicit broadcasting 2 sets of dims';
$pb->make_physical();
$pc->make_physical();
maximum($pa->broadcast(0,1),$pc);
cmp_ok($pb->at(0,0), '==', 10, 'at(0,0)');
cmp_ok($pb->at(1,1), '==', 14, 'at(1,1)');
minimum($pa->broadcast(0,1),$pb->broadcast(0,1));
cmp_ok($pb->at(0,0), '==', 0, 'at(0,0)');
cmp_ok($pb->at(1,1), '==', 4, 'at(1,1)');
}

{
# Now, test 'unbroadcast'.
my $pa = zeroes(4,5,6);
my $pb = $pa->broadcast(1);
my $pc = $pb->unbroadcast(2);
is(join(',',$pc->dims), "4,6,5", 'unbroadcast dims');
# $pb->jdump; $pc->jdump;
}

{
#### Now, test whether the Perl-accessible broadcast works:
my $pa = pdl [[0,1,2],[3,4,5],[6,7,8]],[[10,11,12],[13,14,15],[16,17,18]];
my $pb = pdl [2,3,4];
PDL::broadcastover_n(sub {print "ROUND: @_\n"},$pa,$pb);
# As well as with virtuals...
PDL::broadcastover_n(sub {print "ROUND: @_\n"},$pa->slice("-1:0,-1:0"),$pb);
}

done_testing;
