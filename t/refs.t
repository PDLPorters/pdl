
# This test case points out a problem in the freeing
# of used memory in 1.90_01

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

print "1..2\n";

if(1) {

{
sub ap {
	my($a,$b) = @_;
	my $c = abs($a-$b);
	my $d = max($c);
	1;
}

my $a = pdl (1,2);
my $b = pdl [[1,2],[1,2],[1,2]];
my $c = $a->slice(',*3');
$c->make_physical;
ap($b,$c);
$c = $b->clump(2);

$b->make_physical;
$c->make_physical;
}

ok(1,1);

}



$a =  zeroes 4,5;

$b = $a->slice('1:3:2,2:4:2');

$b .=  ones(2,2);

print $a;

ok(2,1);
