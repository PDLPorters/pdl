# Test conversions. This is not yet good enough: we need
# nasty test cases,

# 1.9901 - converted to new type semantics + extra test

use PDL::LiteF;

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub approx {
	my($a,$b) = @_;
	my $c = abs($a-$b);
	my $d = max($c);
	$d < 0.01;
}

print "1..7\n";

$a = pdl 42.4;

ok(1,$a->get_datatype == 5);

$b = byte $a;

ok(2,$b->get_datatype == 0);
ok(3,$b->at() == 42);

$c = $b * 3;
ok(4,$c->get_datatype == 0); # $c is the same

$d = $b * 600.0;
ok(5,$d->get_datatype == 4); # $d is promoted to float

$pi = 4*atan2(1,1);

$e = $b * $pi;
ok(6,$e->get_datatype == 5); # $e needs to be double to represent result

$f = $b * "-2.2";
ok(7,$f->get_datatype == 5); # $e check strings are handled ok



