
use PDL::LiteF;

$|=1;
kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

print "1..7\n";

if(1) {
$a = pdl [2,3,4],[5,6,7];
$a->doflow;

#print $a;
# $a->jdump;

$a2 = pdl 1;

$b = $a + $a2;
ok(1,"$b" eq <<END);

[
 [3 4 5]
 [6 7 8]
]
END

$c = $b * 2; # This should stay the same flowed structure.

ok(2,"$c" eq <<END);

[
 [ 6  8 10]
 [12 14 16]
]
END
}

# Then, the more difficult ways: explicit threading.

# Dims: 3,3,2
$a = pdl [[0,1,2],[3,4,5],[6,7,8]],[[10,11,12],[13,14,15],[16,17,18]];
# print $a;

$b = zeroes(3,3);
$c = $b->thread(0,1);
$b->make_physical();
$c->make_physical();

# print "B:\n"; $b->dump(); print "C:\n";$c->dump();

maximum($a->thread(0,1),$c);
# print "B:\n"; $b->dump(); print "C:\n";$c->dump();
# print $b;

ok(3,$b->at(0,0) == 10);
ok(4,$b->at(1,1) == 14);

# print "B:\n"; $b->dump(); print "C:\n";$c->dump();
minimum($a->thread(0,1),$b->thread(0,1));
# print $b;

ok(5,$b->at(0,0) == 0);
ok(6,$b->at(1,1) == 4);


# Now, test 'unthread'.
$a = zeroes(4,5,6);
$b = $a->thread(1);
$c = $b->unthread(2);

ok(7,(join ',',$c->dims) eq "4,6,5");

# $b->jdump; $c->jdump;

#### Now, test whether the Perl-accessible thread works:

$a = pdl [[0,1,2],[3,4,5],[6,7,8]],[[10,11,12],[13,14,15],[16,17,18]];
$b = pdl [2,3,4];

PDL::threadover_n($a,$b,sub {print "ROUND: @_\n"});

# As well as with virtuals...

PDL::threadover_n($a->slice("-1:0,-1:0"),$b,sub {print "ROUND: @_\n"});
