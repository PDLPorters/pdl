# PDL interface to GSL differentation routines
# Makefile.PL for a package defined by PP code.

use ExtUtils::MakeMaker;
PDL::Core::Dev->import();

my $msg = undef;
my $forcebuild=0;
my $skip = 0;

# this Makefile uses get_gsl_libs which is defined in
# the parent Makefile.PL

sub gsl_integ_links_ok {
  my($lib,$inc) = @_;
  return defined($lib) && defined($inc) &&
    trylink 'gsl numerical integration libraries',
      << 'EOI',
#include <math.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_integration.h>

double f (double x, void * params)
{
  return pow (x, 1.5);
}

EOI
	<< 'EOB', $lib, $inc;
  gsl_function F;
  double result, abserr;
  int ierr, neval;

  F.function = &f;
  F.params = 0;
 
  ierr = gsl_integration_qng(&F,1.0,2.0,0.0,1e-6,&result,&abserr,&neval);

EOB
}

if (defined $PDL_CONFIG{WITH_GSL} && $PDL_CONFIG{WITH_GSL}==0) {
  $msg = "\n   Will skip build of PDL::GSL::INTEG on this system   \n";
  $skip = 1;
} elsif (defined $PDL_CONFIG{WITH_GSL} && $PDL_CONFIG{WITH_GSL}==1) {
  print "\n   Will forcibly try and build PDL::GSL::INTEG on this system   \n\n";
  $forcebuild=1;
}

if ($^O =~ /win32/i) {
  $msg = "\n\tWin32 systems not yet supported. Will not build PDL::GSL::INTEG\n";
  $skip=1 unless $forcebuild;
}

if (($skip && !$forcebuild) ||
    !gsl_integ_links_ok($GSL_libs, $GSL_includes)) {
  warn "trying to force GSL build but link test failed\n".
    "\t -- aborting GSL build\n" if $forcebuild;
  $msg ||=
    "\n GSL Libraries not found... Skipping build of PDL::GSL::INTEG.\n";
  warn $msg . "\n";
  $msg =~ s/\n//g;
  write_dummy_make( $msg );
  return;
} else {
  print "\n   Building PDL::GSL::INTEG.", 
    "Turn off WITH_GSL if there are any problems\n\n";
}

@pack = (["gsl_integ.pd",INTEG,PDL::GSL::INTEG]);
%hash = pdlpp_stdargs_int(@::pack);

$hash{INC} .= " $GSL_includes";
push @{$hash{LIBS}},$GSL_libs;
WriteMakefile(%hash);


sub MY::postamble {
        pdlpp_postamble_int(@::pack);
}  # Add genpp rule
