use PDL::LiteF;
use PDL::Fit::Polynomial;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.
print "1..1\n";

sub ok {
        my $no = shift ;
        my $result = shift ;
        print "not " unless $result ;
        print "ok $no\n" ;
}

$x = sequence(20)-10;
$y = 30-2*$x+3*$x**2-2*$x**3;

srand(42);
$y += grandom($y)*100;

#points $x,$y;

$yfit = fitpoly1d $x,$y,4;

#hold; line $x, $yfit;

ok(1, max(abs($y-$yfit)) < 200);
