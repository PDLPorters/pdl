use Test::More tests => 3;

use strict;
use warnings;
use PDL;

# all calls to functions that handle finding minimum and maximum should return
# the same values (i.e., BAD).  NOTE: The problem is that perl scalar values
# have no 'BAD' values while pdls do.  We need to sort out and document the
# differences between routines that return perl scalars and those that return
# pdls.
#
SKIP: {
    skip 'Skipped: testing BAD values for minmax', 3 unless $PDL::Config{WITH_BADVAL};

    my $bad_0dim = pdl(q|BAD|);

    TODO: {
       local $TODO = "minmax and minmaximum don't return consistent values";
       is( $bad_0dim->min, 'BAD', "does min returns 'BAD'" );
       is( ($bad_0dim->minmax)[0],  $bad_0dim->min, "does minmax return same as min" );
       is( ($bad_0dim->minmaximum)[0],  $bad_0dim->min, "does minmaximum return same as min" );
    }
}
