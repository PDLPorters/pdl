use PDL::LiteF;
use PDL::Types;
use PDL::Graphics::LUT;

sub ok {
        my $no = shift ;
        my $result = shift ;
        print "not " unless $result ;
        print "ok $no\n" ;
}

sub approx {
        my($a,$b) = @_;
        my $c = abs($a-$b);
        my $d = max($c);
        $d < 0.0001;
}

print "1..5\n";
$testNo = 1;

@names = lut_names();
ok($testNo++, $#names > -1 );

@cols = lut_data( $names[0] );
ok($testNo++, $#cols == 3 );
ok($testNo++, $cols[0]->nelem == $cols[1]->nelem );
ok($testNo++, $cols[2]->get_datatype == $PDL_F );

# check we can reverse things
@cols2 = lut_data( $names[0], 1 );
ok($testNo++, approx($cols[3]->slice('-1:0'),$cols2[3]) );


