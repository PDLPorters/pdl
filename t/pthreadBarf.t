no warnings qw(misc);

# These tests check for proper deferred handling of barf and warn messages when pthreading.
#   
use PDL::LiteF;
use PDL::Image2D;
use Test::More;

use strict;

if (PDL::Core::pthreads_enabled) {
   plan tests => 2;
} else {
   plan tests => 2;
   diag "Control test: pthreads not enabled";
}

## Check Handling of barf messages when pthreading ###

# These statements will cause pthread to happen in two pthreads
set_autopthread_targ(2);
set_autopthread_size(0);

# Because of the duplicate 8's interpolates barf (in the PPcode) will get
#  called. This should not cause a segfault
my $x = float( [1, 2, 3, 4, 5, 8, 9, 10], [1, 2, 3, 4, 5, 8, 8, 8] );
my $y = ($x * 3) * ($x - 2);

# Setup to silence warning messages
local $SIG{__WARN__} = sub {  }; 
# Catch barf messages by running in eval:
eval{
   my ( $ans, $err ) = interpolate(8.5, $x, $y );
};

ok( $@ =~ /identical abscissas/ , "interpolate barf" )
   or diag "Error message should  be 'identical abscissas': got\n>>>$@<<<\n";

## Now Check Warning Messages with pthreading ###

# Create an array of 2 bogus polygon indexes (bogus due to negative indexes)
#  Thes will make polyfill emit a warning message.

# Single polygon
my $poly = pdl([-1,1], [0,0]);
$poly = $poly->reorder(1,0);

# make second polygon have same indexes
my $poly2 = $poly->copy;
$poly = cat $poly, $poly2;

my $mask = zeroes(5,5);

#kill 'INT',$$;
# Because of the negative indexes, a warning message
#   will be printed, which will cause segfault wheen pthreaded, if messages not deferred
#    properly

# Setup to catch warning messages
local $SIG{__WARN__} = sub { die $_[0] }; 

eval{
   polyfill($mask, $poly, 1);
};

ok( $@ =~ /errors during polygonfilling/ , "polyfill barf" )
   or diag "Error message should  be 'errors during polygonfilling': got\n>>>$@<<<\n";
