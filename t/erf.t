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

ok(1, approx(erf(0.),0.) && approx(erf(30.),1.));
ok(2, approx(erf(0.5),1.-erfc(0.5)));
ok(3, approx(erf(erfi(0.5)),0.5) && approx(erfi(erf(0.5)),0.5));
