use PDL::LiteF;
use PDL::Tests;
use PDL::Types;
use PDL::Dbg;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub approx {
	my($a,$b,$c,$d) = @_;
	$c = abs($a-$b);
	$d = max($c);
	return $d < 0.01;
}

print "1..23\n";

$a = xvals(zeroes(byte, 2, 4));

# $P() affine tests
foop($a,($b=null));
print "$b\n";
ok(1,approx($a,$b));

foop($a->xchg(0,1),($b=null));
print "$b\n";
ok(2,approx($a->xchg(0,1),$b));

$vaff = $a->dummy(2,3)->xchg(1,2);
print $vaff;
$vaff->dump;
foop($vaff,($b=null));
ok(3,approx($vaff,$b));

# double qualifier
$a = ones(byte,3000);
dsumover($a,($b=null));
ok(4,$b->get_datatype == $PDL_D);
ok(5,$b->at == 3000);

# float qualifier
fsumover($a,($b=null));
ok(6,$b->get_datatype == $PDL_F);
ok(7,$b->at == 3000);

# int+ qualifier
$i=8;
for (byte,short,ushort,long,float,double) {
  $a = ones($_,3000);
  nsumover($a,($b=null));
  ok($i++,$b->get_datatype == (($PDL_L > $_->[0]) ? $PDL_L : $_->[0]));
  ok($i++,$b->at == 3000);
}

setdim(($a=null),10);
ok(20,join(',',$a->dims) eq "10");
ok(21,approx($a,sequence(10)));

# this used to segv under solaris according to Karl
{ local $=0; # To suppress warnings of use of uninitialized value.
  $ny=7;
  $a = double xvals zeroes (20,$ny);
  print $a,"\n";
  fooseg $a, $b=null;
  print $b,"\n";

  ok(22,1);  # if we get here at all that is alright
  ok(23,approx($a,$b));
}
