# PDL interface to GSL RNG and randist
# Makefile.PL for a package defined by PP code.

use ExtUtils::MakeMaker;

PDL::Core::Dev->import();

$msg = ""; $forcebuild=0;

if (defined $PDL_CONFIG{WITH_GSL} && $PDL_CONFIG{WITH_GSL}==0) {
  $msg = "\n   Will skip build of PDL::GSLSF on this system   \n";
  goto skip;
}

if (defined $PDL_CONFIG{WITH_GSL} && $PDL_CONFIG{WITH_GSL}==1) {
  print "\n   Will forcibly try and build PDL::GSLSF on this system   \n\n";
  $forcebuild=1;
}

if ($^O =~ /win32/i) {
  $msg = "\n\tWin32 systems not yet supported. Will not build PDL::GSLSF\n";
  goto skip unless $forcebuild;
}

$donot = 0;

# check for the old style flags
warn << 'EOW' if ref $PDL_CONFIG{GSL_LIBS};
The GSL_LIBS config variable must be a string (!)
 not a reference. You should probably leave it undefined
 and rely on gsl-config. Build will likely fail.
EOW

my $lib = ($PDL_CONFIG{GSL_LIBS} or
  `gsl-config --libs` or
  warn "\tno GSL link info (libgsl probably not available)\n");
my $inc = ($PDL_CONFIG{GSL_INC} or
  `gsl-config --cflags` or 
  warn "\tno GSL include info (libgsl probably not available)\n\n");
chomp $lib; chomp $inc;

$donot = 1 unless defined $lib && defined $inc &&
  trylink 'gsl libraries',
  << 'EOI',
#include <gsl/gsl_sf_bessel.h>
EOI
  << 'EOB', $lib, $inc;

  double x = 5.0;
  double expected = -0.17759677131433830434739701;
  
  double y = gsl_sf_bessel_J0 (x);

  return 0;

EOB
if ($donot) {
  $msg =  "\n GSL Libraries not found... Skipping build of PDL::GSLSF.\n";
  warn "trying to force GSL build but link test failed\n".
    "\t -- aborting GSL build\n" if $forcebuild;
}

skip:

	if ($msg ne "") {
	  warn $msg . "\n";
	  $msg =~ s/\n//g;
	  write_dummy_make( $msg );
	  $donot = 1;
	} else {
	  print "\n   Building PDL::GSLSF.", 
	  "Turn off WITH_GSL if there are any problems\n\n";
	}


return if $donot;

$GSL_includes = $inc;
$GSL_libs = $lib;

# Note Slatec now handles f77 availability itself

WriteMakefile(
	'NAME' => 'PDL::GSLSF',
	VERSION => '0.5',
	# VERSION_FROM => '../../Basic/Core/Version.pm',
       #DIR =>  [ qw/airy bessel chebyshev clausen coulomb coupling dawson debye dilog elementary ellint elljac erf exp expint fermi_dirac gamma gegenbauer hyperg laguerre legendre log poly pow_int psi synchrotron transport trig zeta/ ],
);

