# XXX SOME TESTS DISABLED

use PDL::LiteF;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub ok {
	my $no = shift ;
	my $result = shift ;
	if($ENV{PDL_T}) {
		if($result) { print "ok $no\n";return }
		my ($p,$f,$l) = caller;
		print "FAILED TEST $no AT $p $f $l\n";
	} else {
		print "not " unless $result ;
		print "ok $no\n" ;
	}
}

# XXX

print "1..33\n";

if(1) {

{my ($a,$b,$c);

# 1. Test that changes do flow

$a = pdl 2,3,4;

$a->doflow;

$b = $a + $a;

ok(1,($b->at(0) == 4));
ok(2,($b->at(1) == 6));

$a->set(0,50);

ok(3,($b->at(0) == 100));
ok(4,($b->at(1) == 6));

# 2. If we don't want flow, we mustn't have it.

$a = pdl 2,3,4;

$b = $a + $a;

ok(5,($b->at(0) == 4));
ok(6,($b->at(1) == 6));

$a->set(0,50);

ok(7,($b->at(0) == 4));
ok(8,($b->at(1) == 6));

$ind = 9;

# 3. Test what happens when we assign to $b. (no coredumps allowed)

$a = pdl 2,3,4;

$a->doflow;

$b = $a + $a;

ok($ind++,($b->at(0) == 4));
ok($ind++,($b->at(1) == 6));

$b->set(0,50); # This must break the dataflow completely

ok($ind++,($b->at(0) == 50));
ok($ind++,($b->at(1) == 6));
ok($ind++,($a->at(0) == 2));
ok($ind++,($a->at(1) == 3));

$a->set(0,33);

ok($ind++,($b->at(0) == 50));
ok($ind++,($b->at(1) == 6));
ok($ind++,($a->at(0) == 33));
ok($ind++,($a->at(1) == 3));

# 4. Now a basic slice test. Once Incs etc. are back, need
# to do this also with other kinds of slices.

# This gets so hairy that we want to use strings for testing.

$a = pdl [2,3,4],[5,6,7];

ok($ind++, ("$a" eq <<END));

[
 [2 3 4]
 [5 6 7]
]
END

$b = $a->slice('1:2,:');
ok($ind++, ("$b" eq <<END));

[
 [3 4]
 [6 7]
]
END

$a->set(1,1,9);
ok($ind++, ("$a" eq <<END));

[
 [2 3 4]
 [5 9 7]
]
END

ok($ind++, ("$b" eq <<END));

[
 [3 4]
 [9 7]
]
END

$c = $a->slice('0:1,:');
ok($ind++, ("$c" eq <<END));

[
 [2 3]
 [5 9]
]
END

$b->set(0,0,8);

ok($ind++, ("$a" eq <<END));

[
 [2 8 4]
 [5 9 7]
]
END

ok($ind++, ("$b" eq <<END));

[
 [8 4]
 [9 7]
]
END

ok($ind++, ("$c" eq <<END));

[
 [2 8]
 [5 9]
]
END
}

# 5. Now, to the hairy stuff of generations and progenitors.

# XXX DISABLED
if(0) {my($a,$a2,$b,$c,$d,$e,$f,$g,@ps);

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

$a = pdl [2,3,4],[5,6,7];
$a->doflow;

$b = $a + 1;

ok($ind++, ("$b" eq <<END));

[
 [3 4 5]
 [6 7 8]
]
END


#print $b;

# $foo2 = pdl 2;

$c = $b * 2; # This should stay the same flowed structure.

ok($ind++, ("$c" eq <<END));

[
 [ 6  8 10]
 [12 14 16]
]
END

# print $c;

$d = $b->slice('1:2,:');
$e = $d->slice('1,:');

# NOW

#print "DDUMP1\n";
# $d->jdump();

$d += 0.5;

#print "DDUMP2\n";
# $d->jdump();

# print $d;
# $d->jdump();

$f = $b * 2;

# This checks whether the system realizes to look for the new $e.
$g = $e - 15;

# print $a,$b,$c,$d,$e,$f,$g;

$a->set(0,0,8);
$a->set(1,0,9);
$a->set(2,0,10);
@ps = ($a,$b,$c,$d,$e,$f,$g);

# print "PRINTS\n"; $b->jdump;
# $c->jdump;

#map {if($_) {# $_->jdump;
#	print $_} else {print "FOO\n";}} @ps;

undef @ps;

ok($ind++, ("$a" eq <<END));

[
 [ 8  9 10]
 [ 5  6  7]
]
END

ok($ind++, ("$b" eq <<END));

[
 [   9 10.5 11.5]
 [   6  7.5  8.5]
]
END

ok($ind++, ("$c" eq <<END));

[
 [18 20 22]
 [12 14 16]
]
END

ok($ind++, ("$d" eq <<END));

[
 [10.5 11.5]
 [ 7.5  8.5]
]
END

ok($ind++, ("$e" eq <<END));

[
 [11.5]
 [ 8.5]
]
END

ok($ind++, ("$f" eq <<END));

[
 [18 21 23]
 [12 15 17]
]
END

ok($ind++, ("$g" eq <<END));

[
 [-3.5]
 [-6.5]
]
END


}
}

# 6. Now, what if the mutated one is actually the parent.
if(0) { # XXX DISABLED
	my($a,$b,$c,$d);
	$a = pdl 2,3,4;
	$a->doflow;
	$a2 = pdl 2;
	$b = $a * $a2;

#	print $b;

ok($ind++, ("$b" eq "[4 6 8]"));

#	$b->jdump;

	$c = pdl 1;
	$b += $c;
#	$b->jdump;
#	$c->jdump;

#	print $b;
ok($ind++, ("$b" eq "[5 7 9]"));
#	$b->jdump;

#	print "TOSETA\n";
	$a->set(1,5);
#	print "TODUMPA\n";
#	$a->jdump();
#	$b->jdump();
#	print "TOPRINTB\n";
#	print $b;
ok($ind++, ("$b" eq "[5 11 9]"));

#	print "EXITING SCOPE\n";

}
#print "EXITED SCOPE\n";

# 7. What about axisvals:
{
	my($a,$b);
	$a = zeroes 5,3;

#	print $a;

ok($ind++, ("$a" eq <<END));

[
 [0 0 0 0 0]
 [0 0 0 0 0]
 [0 0 0 0 0]
]
END


#	print "NEW_OR_INPLACE_NOW\n";
	$b = PDL::Core::new_or_inplace($a);
#	print "NEW_OR_INPLACE_DONE\n";
#	$b->jdump();
	$c = $b->xchg(0,1);

#	$c->jdump();
	$c->make_physical();
#	$c->jdump();

	axisvalues($c);

#	print $c;

ok($ind++, ("$c" eq <<END));

[
 [0 1 2]
 [0 1 2]
 [0 1 2]
 [0 1 2]
 [0 1 2]
]
END



#	print $b;

ok($ind++, ("$b" eq <<END));

[
 [0 0 0 0 0]
 [1 1 1 1 1]
 [2 2 2 2 2]
]
END

#	print $a;

ok($ind++, ("$a" eq <<END));

[
 [0 0 0 0 0]
 [0 0 0 0 0]
 [0 0 0 0 0]
]
END


#	$b->jdump;
#	print $b;
#
#	$b = axisvalues($a);
#
#	print $b;

#       warn "Two tests disabled (31-32) as do not work\n";

       if(1) { # These tests diaabled (do not work) XXX Do

         $a = zeroes 5,5;
         $b = $a->slice("1:3,1:3");
         my $c = $b->slice("(1),(1)");
         ok($ind++,($c->at() == 0));
         $a .= 1;
         ok($ind++,($c->at() == 1));
         $a .= 2;
         ok($ind++,($c->at() == 2));
       }

}

exit 0;

# print "DONE\n";
