# tests for error checking of input args to PP compiled function
#
use PDL::LiteF;
use vars qw/$a $b/;
kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub ok {
        my $no = shift ;
        my $result = shift ;
        print "not " unless $result ;
        print "ok $no\n" ;
}

sub tapprox {
        my($a,$b,$c,$d) = @_;
        $c = abs($a-$b);
        $d = max($c);
        return $d < 0.01;
}

print "1..1\n";

$b=pdl([1,2,3])->long;
$a=[1,2,3];
eval 'PDL::Ufunc::sumover($a,$b)';

print "EXPECT ERROR NEXT:\n-----\n";
print $@;
print "-----\n";

ok(1,$@ =~ /Error - argument is not a recognised data structure/);


