use Test::More tests => 7;
use PDL::LiteF;
use Test::Exception;

use strict;
use warnings;

my $pa = pdl [[ 1,  2,  3,  0],
      [ 1, -1,  2,  7],
      [ 1,  0,  0,  1]];

my $pb = pdl [[1, 1],
     [0, 2],
     [0, 2],
     [1, 1]];

my $pc = pdl [[ 1, 11],
      [ 8, 10],
      [ 2,  2]];

my $res = $pa x $pb;

ok(all approx($pc,$res));

my $eq = float [[1,1,1,1]];  # a 4,1-matrix ( 1 1 1 1 )

# Check collapse: output should be a 1x2...
ok(all approx($eq x $pb  , pdl([[2,6]]) )); # ([4x1] x [2x4] -> [1x2])

# Check dimensional exception: mismatched dims should throw an error
dies_ok {
	my $pz = $pb x $eq; # [2x4] x [4x1] --> error (2 != 1)
};

{
# Check automatic scalar multiplication
my $pz;
lives_ok { $pz = $pb x 2; };
ok( all approx($pz,$pb * 2));
}

{
my $pz;
lives_ok { $pz = pdl(3) x $pb; };
ok( all approx($pz,$pb * 3));
}
