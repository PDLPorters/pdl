# Test of the NDF I/O system
# Requires that the NDF module is available.

use PDL::LiteF;
use strict;
$PDL::verbose = 1;
my $loaded;

# Check that we can load the module
BEGIN {
  # Kluge loading to force NDF module to be loaded now.
  # This is required since currently the PDL::IO::NDF module
  # only loads the NDF module when required.
  eval " use PDL::IO::NDF; use NDF";
  $loaded = ( $@ ? 0 : 1 );
}

# Test counters
my $plan = 6;
my $n = 0;

kill 'INT',$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.


# Simple okay test
sub ok {
        $n++; # increment test counter
        my $result = shift ;
        print "not " unless $result ;
        print "ok $n\n" ;

}


print "1..$plan\n";
unless ($loaded) {
  for (1..$plan) {
    print "ok $_ # Skipped: PDL::IO::NDF requires the NDF module.\n";
  }
  exit;
}


# Now start by creating a test PDL
my $pdl = pdl( 1,5,10,8);

# Now add a header
$pdl->sethdr(  { NDFTEST => 'yes' } );

# output file name
my $ndffile = "test.sdf";

# Write it out to disk
$pdl->wndf( $ndffile );


# Set up an END block to remove the file
END {
  unlink $ndffile if defined $ndffile and -e $ndffile;
}

# Now read it back in
my $in = rndf( $ndffile );

# Compare the number of entries
ok( $in->dims == $pdl->dims  );

# Check each entry
my $range = $pdl->getdim(0) - 1;
foreach ( 0 .. $range ) {
  ok( $in->at($_) == $pdl->at($_))
}

# Now compare headers
ok( $pdl->gethdr->{NDFTEST} eq $in->gethdr->{NDFTEST} );


