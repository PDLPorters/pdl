
use PDL::LiteF;
# PDL::Core::set_debugging(1);
kill INT,$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub tapprox {
	my($a,$b,$c,$d) = @_;
	$c = abs($a-$b);
	$d = max($c);
	$d < 0.01;
}

sub cleanup {
    my $h = shift;
    unlink $h, $h . ".hdr";
}

use PDL;
use PDL::IO::FastRaw;
use PDL::Config;

my $tmpdir = $PDL::Config{TEMPDIR};

print "1..5\n";

$a = pdl [2,3],[4,5],[6,7];

print $a;

my $name = $tmpdir . "/tmp0";

cleanup $name;

writefraw($a,$name);

$b = readfraw($name);

print $b;

$b->dump;

ok(1,tapprox($a,$b));

undef $b;

if ($^O =~ /win32/i) {
    for (2..5) {
        print "ok $_ # Skipped: no mmap support on win32 yet.\n";
    }
  exit;
}

$c = mapfraw($name);

print $c;

ok(2,tapprox($a,$c));

$c += 1;

print $c;

undef $c;

$b = readfraw($name);

print $b;

ok(3,tapprox($a+1,$b));

cleanup $name;

$name = $tmpdir . "/tmp1";
$e = mapfraw($name, {Creat => 1, Datatype => &float, Dims => [3,2]});

print $e;
$e += xvals $e;
$e += 0.1 * yvals $e;

undef $e;

$f = readfraw($name);

ok(4,tapprox($f, PDL->pdl([[0,1,2],[0.1,1.1,2.1]])));

ok(5, $f->type->[0] == (&float)->[0]);

cleanup $name;


