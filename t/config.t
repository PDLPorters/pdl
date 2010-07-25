#!/usr/bin/perl
#
# Verify that the Config.pm values were updated from the
# actual build process.  Quick placeholder tests for now.
# Eventually need to check that the configuration matches
# the result of use_ok or some such.

BEGIN {
   use Test::More;
}

BEGIN {
   use_ok( 'PDL::Config' );
}

SKIP: {
   # This is Known_problems bug sf.net #3030998
   # PDL::Config does not match actual build configuration
   
   # generate list of WITH_* keys from PDL::Config
   my @keys = grep { /^WITH_/ } keys %PDL::Config;

   if ($PDL::Config{SKIP_KNOWN_PROBLEMS} or exists $ENV{SKIP_KNOWN_PROBLEMS} ) {
      skip('Known_problem sf.net bug #3030998', scalar(@keys))
   }

   foreach my $key ( @keys ) {
      # there should be no undef values
      ok( defined $PDL::Config{$key} , "check $key in Config.pm" );
   }
}

done_testing();
