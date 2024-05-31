use strict;
use warnings;
use Test::More;
use PDL::LiteF;

{
  my $pa = pdl 2,3,4;
  $pa->doflow;
  my $pb = $pa + $pa;
  is "$pb", '[4 6 8]';
  $pa->set(0,50);
  is "$pb", '[100 6 8]';
}

{
	# 2. If we don't want flow, we mustn't have it.
	my $pa = pdl 2,3,4;
	my $pb = $pa + $pa;
	is($pb->at(0), 4);
	is($pb->at(1), 6);
	$pa->set(0,50);
	is($pb->at(0), 4);
	is($pb->at(1), 6);
}

{
	# 3. Test what happens when we assign to $pb. (no coredumps allowed)
	my $pa = pdl 2,3,4;
	$pa->doflow;
	my $pb = $pa + $pa;
	is($pb->at(0), 4);
	is($pb->at(1), 6);
	$pb->set(0,50);
	$pb->sever; # As of 2.064 you must break the dataflow manually
	is($pb->at(0), 50);
	is($pb->at(1), 6);
	is($pa->at(0), 2);
	is($pa->at(1), 3);
	$pa->set(0,33);
	is($pb->at(0), 50);
	is($pb->at(1), 6);
	is($pa->at(0), 33);
	is($pa->at(1), 3);
}

{
	# 4. Now a basic slice test. Once Incs etc. are back, need
	# to do this also with other kinds of slices.
	# This gets so hairy that we want to use strings for testing.
	my $pa = pdl [2,3,4],[5,6,7];
	is("$pa", <<END);

[
 [2 3 4]
 [5 6 7]
]
END
	my $pb = $pa->slice('1:2,:');
	is("$pb", <<END);

[
 [3 4]
 [6 7]
]
END
	$pa->set(1,1,9);
	is("$pa", <<END);

[
 [2 3 4]
 [5 9 7]
]
END
	is("$pb", <<END);

[
 [3 4]
 [9 7]
]
END
	my $pc = $pa->slice('0:1,:');
	is("$pc", <<END);

[
 [2 3]
 [5 9]
]
END
	$pb->set(0,0,8);
	is("$pa", <<END);

[
 [2 8 4]
 [5 9 7]
]
END
	is("$pb", <<END);

[
 [8 4]
 [9 7]
]
END
	is("$pc", <<END);

[
 [2 8]
 [5 9]
]
END
}

# 5. Now, to the hairy stuff of generations and progenitors.

SKIP: { # XXX DISABLED
	if( 0 ) {
	my($pa,$a2,$pb,$pc,$pd,$pe,$pf,$pg,@ps);
# We set up the following dependency graph:
#
#       c
#       ^
#       |
#  a -> b . . . > b' -> f
#       |         |
#       V         V
#       d - - - > d'
#       |         |
#       V         V
#       e . . . > e' -> g
#
# which, although it does not exercise *every* code path, still
# does a lot.
	$pa = pdl [2,3,4],[5,6,7];
	$pa->doflow;
	$pb = $pa + 1;
	is("$pb", <<END);

[
 [3 4 5]
 [6 7 8]
]
END
	$pc = $pb * 2; # This should stay the same flowed structure.
	is("$pc", <<END);

[
 [ 6  8 10]
 [12 14 16]
]
END
	$pd = $pb->slice('1:2,:');
	$pe = $pd->slice('1,:');
	$pd += 0.5;
	$pf = $pb * 2;
	# This checks whether the system realizes to look for the new $pe.
	$pg = $pe - 15;
	$pa->set(0,0,8);
	$pa->set(1,0,9);
	$pa->set(2,0,10);
	@ps = ($pa,$pb,$pc,$pd,$pe,$pf,$pg);
	undef @ps;
	is("$pa", <<END);

[
 [ 8  9 10]
 [ 5  6  7]
]
END
	is("$pb", <<END);

[
 [   9 10.5 11.5]
 [   6  7.5  8.5]
]
END
	is("$pc", <<END);

[
 [18 20 22]
 [12 14 16]
]
END
	is("$pd", <<END);

[
 [10.5 11.5]
 [ 7.5  8.5]
]
END
	is("$pe", <<END);

[
 [11.5]
 [ 8.5]
]
END
	is("$pf", <<END);

[
 [18 21 23]
 [12 15 17]
]
END
	is("$pg", <<END);

[
 [-3.5]
 [-6.5]
]
END
	}
}

SKIP: { # XXX DISABLED
# 6. Now, what if the mutated one is actually the parent.
	if(0) {
		my($pa,$pb,$pc,$pd);
		$pa = pdl 2,3,4;
		$pa->doflow;
		my $a2 = pdl 2;
		$pb = $pa * $a2;

		is("$pb", "[4 6 8]");

		$pc = pdl 1;
		$pb += $pc;

		is("$pb", "[5 7 9]");

		$pa->set(1,5);
		is("$pb", "[5 11 9]");
	}
}

# 7. What about axisvals:
{
	my $pa = zeroes 5,3;
	is("$pa", <<END);

[
 [0 0 0 0 0]
 [0 0 0 0 0]
 [0 0 0 0 0]
]
END
	my $pb = PDL::Core::new_or_inplace($pa);
	my $pc = $pb->transpose;
	axisvalues($pc->inplace);
	is("$pc", <<END);

[
 [0 1 2]
 [0 1 2]
 [0 1 2]
 [0 1 2]
 [0 1 2]
]
END
	is("$pb", <<END);

[
 [0 0 0 0 0]
 [1 1 1 1 1]
 [2 2 2 2 2]
]
END
	is("$pa", <<END);

[
 [0 0 0 0 0]
 [0 0 0 0 0]
 [0 0 0 0 0]
]
END
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
$pa->doflow;
my $a2 = pdl 1;
my $pb = $pa + $a2;
is("$pb", <<END, 'pb doflow');

[
 [3 4 5]
 [6 7 8]
]
END
my $pc = $pb * 2; # This should stay the same flowed structure.
is("$pc", <<END, 'multiplied');

[
 [ 6  8 10]
 [12 14 16]
]
END
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
