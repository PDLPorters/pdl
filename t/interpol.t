
# Simple test of the interpol routine

use PDL::Lite;


print "1..1\n";  

my $testNo = 1;



my $yvalues =  (new PDL( 0..5))   - 20;

my $xvalues = -(new PDL (0..5))*.5;

my $x = new PDL(-2);

ok( $testNo++,$x->interpol($xvalues,$yvalues) == -16 );



#  Testing utility functions:
sub ok {
        my $no = shift ;
        my $result = shift ;
        print "not " unless $result ;
        print "ok $no\n" ;
}
