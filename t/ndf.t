# -*-perl-*-
# Test of the NDF I/O system
# Requires that the NDF module is available.

use strict;

use Test;

use PDL::LiteF;
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

kill 'INT',$$  if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

unless ( $loaded ) {
    plan tests => 1;
#    for ( 1 .. $ntests ) {
	skip( "Skipped: PDL::IO::NDF requires the NDF module.", 1, 1 );
#    }
    exit;
}

plan tests => 10;

sub tapprox ($$) {
    my ( $a, $b ) = @_;
    return abs($a-$b) <= 1.0e-5;
}

# Now start by creating a test PDL
my $pdl = pdl( 1,5,10,8);

# Now add a header
$pdl->sethdr(  { NDFTEST => 'yes' } );

# output file name
my $ndffile = "test.sdf";
unlink $ndffile if -e $ndffile;

# Write it out to disk
$pdl->wndf( $ndffile );
ok( -e $ndffile );

# Set up an END block to remove the file
END {
  unlink $ndffile if defined $ndffile and -e $ndffile;
}

# Now read it back in
my $in = rndf( $ndffile );

# Compare the number of entries
ok( $in->dims == $pdl->dims );

# Check each entry
my $range = $pdl->getdim(0) - 1;
foreach ( 0 .. $range ) {
  ok( $in->at($_) == $pdl->at($_))
}

# Now compare headers
ok( $pdl->gethdr->{NDFTEST} eq $in->gethdr->{NDFTEST} );

# try a 2D image
$pdl = pdl( [1,5,10],[8,4,-4]);
$pdl->wndf( $ndffile );
$in = rndf( $ndffile );

# Compare the number of entries
ok( $in->dims == $pdl->dims );
ok( tapprox( sum($in - $pdl), 0.0 ) );

# try a subset of the 2D image
# NOTE: NDF starts counting at 1, not 0
$in = rndf( "test(1:2,2)" );
ok( tapprox( sum($in - $pdl->slice('0:1,1') ), 0.0 ) );

# end of test
