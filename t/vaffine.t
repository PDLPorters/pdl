
# Test vaffine optimisation

use PDL::LiteF;

print "1..1\n";

sub ok {
        my $no = shift ;
        my $result = shift ;
        print "not " unless $result ;
        print "ok $no\n" ;
}

$x = zeroes(100,100);

$y = $x->slice('10:90,10:90');

$y++;

ok(1, (not $y->allocated) ) ;


