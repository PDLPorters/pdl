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

sub eprint {
	print "EXPECT ERROR NEXT:\n-----\n";
	print $_[0];
	print "-----\n";
}

print "1..3\n";

$b=pdl([1,2,3])->long;
$a=[1,2,3];
eval 'PDL::Ufunc::sumover($a,$b)';

eprint $@;

ok(1,$@ =~ /Error - argument is not a recognised data structure/);

eval { PDL::Ufunc::sumover({}) };
eprint $@;

ok 2, $@ =~ /Hash given as a pdl - but not {PDL} key/;


$c = 0;
eval { PDL::Ufunc::sumover(\$c) };
eprint $@;

ok 3, $@ =~ /Error - argument is not a recognised data structure/;


