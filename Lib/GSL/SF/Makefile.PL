# PDL interface to GSL Special functions
# Makefile.PL for a package defined by PP code.

use ExtUtils::MakeMaker;
PDL::Core::Dev->import();

sub gsl_sf_links_ok {
  my($lib,$inc) = @_;
  return defined($lib) && defined($inc) &&
  trylink 'gsl SF libraries',
  << 'EOI',
#include <gsl/gsl_sf_bessel.h>
EOI
  << 'EOB', $lib, $inc;

  double x = 5.0;
  double expected = -0.17759677131433830434739701;
  
  double y = gsl_sf_bessel_J0 (x);

  return 0;

EOB
}

$skip = 0;
$msg = undef;
$forcebuild=0;

if (defined $PDL_CONFIG{WITH_GSL} && $PDL_CONFIG{WITH_GSL}==0) {
  $msg = "\n   Will skip build of PDL::GSLSF on this system   \n";
  $skip = 1;
} elsif (defined $PDL_CONFIG{WITH_GSL} && $PDL_CONFIG{WITH_GSL}==1) {
  print "\n   Will forcibly try and build PDL::GSLSF on this system   \n\n";
  $forcebuild=1;
}

if ($^O =~ /win32/i) {
  $msg = "\n\tWin32 systems not yet supported. Will not build PDL::GSLSF\n";
  $skip = 1  unless $forcebuild;
}


if (($skip && !$forcebuild) ||
    !gsl_sf_links_ok($GSL_libs, $GSL_includes)) {
  warn "trying to force GSL build but link test failed\n".
    "\t -- aborting GSL build\n" if $forcebuild;
  $msg ||=
    "\n GSL Libraries not found... Skipping build of PDL::GSLSF.\n";
  warn $msg . "\n";
  $msg =~ s/\n//g;
  write_dummy_make( $msg );
  return;
} else {
  print "\n   Building PDL::GSLSF.", 
    "Turn off WITH_GSL if there are any problems\n\n";
}

WriteMakefile(
	'NAME' => 'PDL::GSLSF',
	VERSION => '0.5',
	# VERSION_FROM => '../../Basic/Core/Version.pm',
       #DIR =>  [ qw/airy bessel chebyshev clausen coulomb coupling dawson debye dilog elementary ellint elljac erf exp expint fermi_dirac gamma gegenbauer hyperg laguerre legendre log poly pow_int psi synchrotron transport trig zeta/ ],
);

