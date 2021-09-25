use strict;
use warnings;
use ExtUtils::MakeMaker;

our ($GSL_includes, $GSL_libs); 

sub gsl_integ_links_ok {
  my($lib,$inc) = @_;
  return defined($lib) && defined($inc) &&
    trylink('',
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
	<< 'EOB', $lib, $inc);
  gsl_function F;
  double result, abserr;
  int ierr, neval;

  F.function = &f;
  F.params = 0;
 
  ierr = gsl_integration_qng(&F,1.0,2.0,0.0,1e-6,&result,&abserr,&neval);
EOB
}

if (!gsl_integ_links_ok($GSL_libs, $GSL_includes)) {
  write_dummy_make( "Skipping build of PDL::GSL::INTEG." );
  return;
}

my @pack = (["gsl_integ.pd", qw(INTEG PDL::GSL::INTEG)]);
my %hash = pdlpp_stdargs_int(@pack);

$hash{INC} .= " $GSL_includes";
push @{$hash{LIBS}},$GSL_libs;
$hash{OBJECT} .= ' FUNC$(OBJ_EXT)';

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };
WriteMakefile(%hash);
