use Test::More tests => 1;
use PDL::LiteF;
use PDL::Math;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub tapprox {
        my($a,$b) = @_;
        $c = abs($a-$b);
        $d = max($c);
        $d < 0.01;
}

ok(tapprox(qsort((polyroots(pdl(1,-55,1320,-18150,157773,-902055,
3416930,-8409500,12753576,-10628640,3628800),zeroes(11)))[0]),1+sequence(10)));
      
