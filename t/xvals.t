use PDL::LiteF;
kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

print "1..4\n";

$a0 = zeroes(3,2);
# $a0->doflow();

 print $a0;

$a1 = $a0->slice('(1)');

 print $a1;

# $a0->dump(); $a1->dump();

# $a1->dump();

$a1 += 4;

# $a1->dump();

 print $a1;

$dummy = PDL::Core::new_or_inplace($a0);
print $dummy;
$dummy2 = $dummy->xchg(0,0);
print $dummy2;
$dummy2->dump();
$dummy->dump();
PDL::Primitive::axisvalues($dummy2);
$dummy2->dump();
$dummy->dump();
print $dummy2;
print $dummy;



# $a1->dump();

# $a0->dump(); $a1->dump();

# print $a1;

# print $a0;

# print $a1;

$a = xvals $a0;

print $a;

ok(1,$a->at(0,0) == 0);
ok(2,$a->at(1,0) == 1);
ok(3,$a->at(2,0) == 2);
ok(4,$a->at(1,1) == 1);

$a = zeroes 5,10;

$b = yvals $a;

$c = $b->copy();

$d = $b-$c;

print "$d,$b,$c";

# print $a;

print "OUTOUT\n";

