use PDL::LiteF;
use PDL::Math;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

print "1..3\n";

sub ok {
        my $no = shift ;
        my $result = shift ;
        print "not " unless $result ;
        print "ok $no\n" ;
}

sub approx {
        my($a,$b) = @_;
        $c = abs($a-$b);
        $d = max($c);
        $d < 0.01;
}

ok(1, approx(sinh(0.3),0.3045) && approx(acosh(42.1),4.43305));
ok(2, approx(acos(0.3),1.2661) && approx(tanh(0.4),0.3799));
ok(3, approx(cosh(2.0),3.7621) && approx(atan(0.6),0.54041));
