use strict;
use warnings;
use Test::More;
use PDL::LiteF;

{
	# 1. Test that changes do flow
	my $pa = pdl 2,3,4;
	$pa->doflow;
	my $pb = $pa + $pa;
	is($pb->at(0), 4);
	is($pb->at(1), 6);
	$pa->set(0,50);
	is($pb->at(0), 100);
	is($pb->at(1), 6);

	# exercise that set_datatype destroys trans
	# XXX if set datatype of $pa, $pb gets a corrupted value of 64.0000158399343
	$pb->set_datatype(PDL::float()->enum);
	$pa->set(0,60);
	is($pb->at(0), 100);
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
	$pb->set(0,50); # This must break the dataflow completely
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
	#note $pb;
	# $foo2 = pdl 2;
	$pc = $pb * 2; # This should stay the same flowed structure.
	is("$pc", <<END);

[
 [ 6  8 10]
 [12 14 16]
]
END
	# note $pc;
	$pd = $pb->slice('1:2,:');
	$pe = $pd->slice('1,:');
	# NOW
	#print "DDUMP1\n";
	# $pd->jdump();
	$pd += 0.5;
	#print "DDUMP2\n";
	# $pd->jdump();
	# print $pd;
	# $pd->jdump();
	$pf = $pb * 2;
	# This checks whether the system realizes to look for the new $pe.
	$pg = $pe - 15;
	# print $pa,$pb,$pc,$pd,$pe,$pf,$pg;
	$pa->set(0,0,8);
	$pa->set(1,0,9);
	$pa->set(2,0,10);
	@ps = ($pa,$pb,$pc,$pd,$pe,$pf,$pg);
	# print "PRINTS\n"; $pb->jdump;
	# $pc->jdump;
	#map {if($_) {# $_->jdump;
	#	print $_} else {print "FOO\n";}} @ps;
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

	#	note $pb;

		is("$pb", "[4 6 8]");

	#	$pb->jdump;

		$pc = pdl 1;
		$pb += $pc;
	#	$pb->jdump;
	#	$pc->jdump;

	#	note $pb;
		is("$pb", "[5 7 9]");
	#	$pb->jdump;

	#	note "TOSETA\n";
		$pa->set(1,5);
	#	note "TODUMPA\n";
	#	$pa->jdump();
	#	$pb->jdump();
	#	note "TOPRINTB\n";
	#	note $pb;
		is("$pb", "[5 11 9]");

	#	print "EXITING SCOPE\n";

	}
}
#print "EXITED SCOPE\n";

# 7. What about axisvals:
{
	my $pa = zeroes 5,3;
#	note $pa;
	is("$pa", <<END);

[
 [0 0 0 0 0]
 [0 0 0 0 0]
 [0 0 0 0 0]
]
END
#	note "NEW_OR_INPLACE_NOW\n";
	my $pb = PDL::Core::new_or_inplace($pa);
#	note "NEW_OR_INPLACE_DONE\n";
#	$pb->jdump();
	my $pc = $pb->transpose;
#	$pc->jdump();
	$pc->make_physical();
#	$pc->jdump();
	axisvalues($pc);
#	note $pc;
	is("$pc", <<END);

[
 [0 1 2]
 [0 1 2]
 [0 1 2]
 [0 1 2]
 [0 1 2]
]
END
#	note $pb;
	is("$pb", <<END);

[
 [0 0 0 0 0]
 [1 1 1 1 1]
 [2 2 2 2 2]
]
END
#	note $pa;
	is("$pa", <<END);

[
 [0 0 0 0 0]
 [0 0 0 0 0]
 [0 0 0 0 0]
]
END
#	$pb->jdump;
#	print $pb;
#	$pb = axisvalues($pa);
#	note $pb;
#       warn "Two tests disabled (31-32) as do not work\n";
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
#print $pa;
# $pa->jdump;
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
# Then, the more difficult ways: explicit threading.
# Dims: 3,3,2
my $pa = pdl [[0,1,2],[3,4,5],[6,7,8]],[[10,11,12],[13,14,15],[16,17,18]];
# print $pa;
my $pb = zeroes(3,3);
my $pc = $pb->thread(0,1);
$pb->make_physical();
$pc->make_physical();
# print "B:\n"; $pb->dump(); print "C:\n";$pc->dump();
maximum($pa->thread(0,1),$pc);
# print "B:\n"; $pb->dump(); print "C:\n";$pc->dump();
# print $pb;
cmp_ok($pb->at(0,0), '==', 10, 'at(0,0)');
cmp_ok($pb->at(1,1), '==', 14, 'at(1,1)');
# print "B:\n"; $pb->dump(); print "C:\n";$pc->dump();
minimum($pa->thread(0,1),$pb->thread(0,1));
# print $pb;
cmp_ok($pb->at(0,0), '==', 0, 'at(0,0)');
cmp_ok($pb->at(1,1), '==', 4, 'at(1,1)');
}

{
# Now, test 'unthread'.
my $pa = zeroes(4,5,6);
my $pb = $pa->thread(1);
my $pc = $pb->unthread(2);
is(join(',',$pc->dims), "4,6,5", 'unthread dims');
# $pb->jdump; $pc->jdump;
}

{
#### Now, test whether the Perl-accessible thread works:
my $pa = pdl [[0,1,2],[3,4,5],[6,7,8]],[[10,11,12],[13,14,15],[16,17,18]];
my $pb = pdl [2,3,4];
PDL::threadover_n($pa,$pb,sub {print "ROUND: @_\n"});
# As well as with virtuals...
PDL::threadover_n($pa->slice("-1:0,-1:0"),$pb,sub {print "ROUND: @_\n"});
}

done_testing;
