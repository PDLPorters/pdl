use strict;
use warnings;
use ExtUtils::MakeMaker;

our ($GSL_includes, $GSL_libs);

sub gsl_diff_links_ok {
  my($lib,$inc) = @_;
  return defined($lib) && defined($inc) &&
    trylink(
      '',
      << 'EOI',
#include <math.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_diff.h>

double f (double x, void * params)
{
  return pow (x, 1.5);
}

EOI
      << 'EOB', $lib, $inc);
  gsl_function F;
  double result, abserr;

  F.function = &f;
  F.params = 0;

  gsl_diff_central (&F, 2.0, &result, &abserr);
  gsl_diff_forward (&F, 0.0, &result, &abserr);
  gsl_diff_backward (&F, 0.0, &result, &abserr);

EOB
}

if (!gsl_diff_links_ok($GSL_libs, $GSL_includes)) {
  write_dummy_make( "Skipping build of PDL::GSL::DIFF." );
  return;
}

my @pack = (["gsl_diff.pd", qw(DIFF PDL::GSL::DIFF)]);
my %hash = pdlpp_stdargs_int(@pack);

$hash{INC} .= " $GSL_includes";
push @{$hash{LIBS}},$GSL_libs;
$hash{OBJECT} .= ' FUNC$(OBJ_EXT)';

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };
WriteMakefile(%hash);
