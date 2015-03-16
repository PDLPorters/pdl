# Test ->*physical*(). This is not yet good enough: we need
# nasty test cases,

use Test::More skip_all => 'Disabled';
use PDL::LiteF;

sub tapprox {
	my($a,$b) = @_;
	$c = abs($a-$b);
	$d = max($c);
	$d < 0.01;
}

plan tests => 6;

$a = zeroes(4,4);
ok($a->isphysical());

$b = xvals $a + 0.1 * yvals $a;
$c = $b->slice("1:3:2,:");
ok(! $c->isphysical());

$d = $b->physical();
ok( $d == $b);

$e = $c->physical();
ok( $e != $c);
ok( $e->isphysical());

ok( tapprox($c,$e));
