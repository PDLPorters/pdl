# -*-perl-*-

use strict;
use Test;

BEGIN {
    plan tests => 8;
}

use PDL::LiteF;
use PDL::Types;
use PDL::Graphics::LUT;

sub tapprox {
    my($a,$b) = @_;
    my $d = max( abs($a-$b) );
    $d < 0.0001;	
}

my @names = lut_names();
ok( $#names > -1, 1 );  # 1

my @cols = lut_data( $names[0] );
ok( $#cols, 3 );                         # 2
ok( $cols[0]->nelem, $cols[1]->nelem );  # 3
ok( $cols[2]->get_datatype, $PDL_F );    # 4

# check we can reverse things
my @cols2 = lut_data( $names[0], 1 );
ok( tapprox($cols[3]->slice('-1:0'),$cols2[3]), 1 );  # 5

# check we know about the intensity ramps
my @ramps = lut_ramps();
ok( $#ramps > -1, 1 ); # 6

# load in a different intensity ramp
my @cols3 = lut_data( $names[0], 0, $ramps[0] ); 
ok( $cols3[0]->nelem, $cols3[1]->nelem ); # 7
ok( tapprox($cols[1],$cols3[1]), 1 );      # 8

