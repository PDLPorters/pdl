# Test conversions. This is not yet good enough: we need
# nasty test cases,

# 1.9901 - converted to new type semantics + extra test

use Test;
BEGIN { plan tests => 7 }

use PDL::LiteF;
use PDL::Types;

sub tapprox {
        my($a,$b) = @_;
        my $c = abs($a-$b);
        my $d = max($c);
        ok($d < 0.01);
}

$a = pdl 42.4;
print "A is $a\n";

ok($a->get_datatype,$PDL_D);

$b = byte $a;
print "B (byte $a) is $b\n";

ok($b->get_datatype,$PDL_B);
ok($b->at(),42);

$c = $b * 3;
ok($c->get_datatype, $PDL_B); # $c is the same
print "C ($b * 3) is $c\n";

$d = $b * 600.0;
ok($d->get_datatype, $PDL_F); # $d is promoted to float
print "D ($b * 600) is $d\n";

$pi = 4*atan2(1,1);

$e = $b * $pi;
ok($e->get_datatype, $PDL_D); # $e needs to be double to represent result
print "E ($b * $pi) is $e\n";

$f = $b * "-2.2";
ok($f->get_datatype, $PDL_D); # $e check strings are handled ok
print "F ($b * string(-2.2)) is $f\n";

