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

ok(1, approx(bessj0(0.5),0.9384) && approx(bessj0(0),1));
ok(2, approx(bessj1(0.1),0.0499) && approx(bessj1(0),0));
ok(3, approx(bessjn(0.8,3),0.010) && approx(bessyn(0.2,2),-32.15714))
