use PDL;
use PDL::ImageND;

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub ok {
        my $no = shift ;
        my $result = shift ;
        print "not " unless $result ;
        print "ok $no\n" ;
}

print "1..1\n";

# Right answer

my $ans = pdl(
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27],
 [ 3,  9, 15, 21, 27, 33, 39, 45, 51, 27]
);

my $a = xvals zeroes 10,3;

my $b = pdl [1,2],[2,1];

my $c=convolve ($a, $b);

ok( 1, int(at(sum($c-$ans)))==0 );


