
use PDL::LiteF;
# PDL::Core::set_debugging(1);
kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

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
	$d < 0.01;
}

use PDL;
use PDL::IO::FastRaw;

print "1..5\n";

$a = pdl [2,3],[4,5],[6,7];

print $a;

unlink "tmp0","tmp0.hdr";

writefraw($a,"tmp0");

$b = readfraw("tmp0");

print $b;

$b->dump;

ok(1,approx($a,$b));

undef $b;

$c = mapfraw("tmp0");

print $c;

ok(2,approx($a,$c));

$c += 1;

print $c;

undef $c;

$b = readfraw("tmp0");

print $b;

ok(3,approx($a+1,$b));

unlink "tmp0","tmp0.hdr";

$e = mapfraw("tmp1", {Creat => 1, Datatype => &float, Dims => [3,2]});

print $e;
$e += xvals $e;
$e += 0.1 * yvals $e;

undef $e;

$f = readfraw("tmp1");

ok(4,approx($f, PDL->pdl([[0,1,2],[0.1,1.1,2.1]])));

ok(5, $f->type->[0] == (&float)->[0]);

# unlink "tmp0","tmp0.hdr";
# unlink "tmp1","tmp1.hdr";


