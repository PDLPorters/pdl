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

print "1..8\n";
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

# check we know about the intensity ramps
@ramps = lut_ramps();
ok($testNo++, $#ramps > -1 );

# load in a different intensity ramp
@cols3 = lut_data( $names[0], 0, $ramps[0] );
ok($testNo++, $cols3[0]->nelem == $cols3[1]->nelem );
ok($testNo++, approx($cols[1],$cols3[1]) );

