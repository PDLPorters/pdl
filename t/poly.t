use PDL::LiteF;
BEGIN {
        eval " use PDL::Fit::Polynomial; ";
        $loaded = ($@ ? 0 : 1);
}


kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.
print "1..1\n";

unless ($loaded) {
        for (1..1) {
                print "ok $_ # Skipped: probably PDL::Slatec not available.\n";
        }
        exit;
}

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

$yfit = fitpoly1d($x,$y,4);

#hold; line $x, $yfit;

ok(1, max(abs($y-$yfit)) < 210);
