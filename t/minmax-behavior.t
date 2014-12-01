use Test::More tests => 3;

use strict;
use warnings;
use PDL;

# all calls to functions that handle finding minimum and maximum should return
# the same values (i.e., BAD)
SKIP: {
    skip 'Skipped: testing BAD values for minmax', 3 unless $PDL::Config{WITH_BADVAL};

    my $bad_0dim = pdl(q|BAD|);

    is( $bad_0dim->min, 'BAD' );
    is( ($bad_0dim->minmax)[0],  $bad_0dim->min );
    is( ($bad_0dim->minmaximum)[0],  $bad_0dim->min );

}
