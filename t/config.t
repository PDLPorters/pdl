# Verify that the Config.pm values were updated from the
# actual build process.

use strict;
use warnings;
use Test::More;
use PDL::Config;

# there should be no undef values
ok( defined $PDL::Config{$_} , "check $_ in Config.pm" )
  for grep { /^WITH_/ } keys %PDL::Config;

done_testing();
