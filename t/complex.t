use PDL::LiteF;
use PDL::Complex;

sub ok {
        my $no = shift ;
        my $result = shift ;
        print "not " unless $result ;
        print "ok $no\n" ;
}

sub tapprox {
        my($a,$b) = @_;
        my $c = abs($a-$b);
        my $d = max($c);
        $d < 0.0001;
}

# more tests required; anybody?
print "1..5\n";
$testNo = 1;

$ref = pdl([[-2,1],[-3,1]]);
$a = i - pdl(2,3);
ok($testNo++, ref $a eq PDL::Complex);
ok($testNo++,tapprox($a->real,$ref));

$a = pdl(2,3) - i;
ok($testNo++, ref $a eq PDL::Complex);
ok($testNo++,tapprox($a->real,-$ref));

# dataflow from complex to real
$ar = $a->real;
$ar++;
ok($testNo++,tapprox($a->real, -$ref+1));
