# PDL interface to GSL differentation routines
# Makefile.PL for a package defined by PP code.

use ExtUtils::MakeMaker;
PDL::Core::Dev->import();

my $msg = undef;
my $forcebuild=0;
my $skip = 0;

# this Makefile uses get_gsl_libs which is defined in
# the parent Makefile.PL

sub gsl_diff_links_ok {
  my($lib,$inc) = @_;
  return defined($lib) && defined($inc) &&
    trylink 'gsl diff libraries',
      << 'EOI',
#include <math.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_diff.h>

double f (double x, void * params)
{
  return pow (x, 1.5);
}

EOI
	<< 'EOB', $lib, $inc;
  gsl_function F;
  double result, abserr;

  F.function = &f;
  F.params = 0;

  gsl_diff_central (&F, 2.0, &result, &abserr);
  gsl_diff_forward (&F, 0.0, &result, &abserr);
  gsl_diff_backward (&F, 0.0, &result, &abserr);

EOB
}

if (defined $PDL_CONFIG{WITH_GSL} && $PDL_CONFIG{WITH_GSL}==0) {
  $msg = "\n   Will skip build of PDL::GSL::DIFF on this system   \n";
  $skip = 1;
} elsif (defined $PDL_CONFIG{WITH_GSL} && $PDL_CONFIG{WITH_GSL}==1) {
  print "\n   Will forcibly try and build PDL::GSL::DIFF on this system   \n\n";
  $forcebuild=1;
}

if ($^O =~ /win32/i) {
  $msg = "\n\tWin32 systems not yet supported. Will not build PDL::GSL::DIFF\n";
  $skip=1 unless $forcebuild;
}

if (($skip && !$forcebuild) ||
    !gsl_diff_links_ok($GSL_libs, $GSL_includes)) {
  warn "trying to force GSL build but link test failed\n".
    "\t -- aborting GSL build\n" if $forcebuild;
  $msg ||=
    "\n GSL Libraries not found... Skipping build of PDL::GSL::DIFF.\n";
  warn $msg . "\n";
  $msg =~ s/\n//g;
  write_dummy_make( $msg );
  return;
} else {
  print "\n   Building PDL::GSL::DIFF.", 
    "Turn off WITH_GSL if there are any problems\n\n";
}

@pack = (["gsl_diff.pd",DIFF,PDL::GSL::DIFF]);
%hash = pdlpp_stdargs_int(@::pack);

$hash{INC} .= " $GSL_includes";
push @{$hash{LIBS}},$GSL_libs;
WriteMakefile(%hash);


sub MY::postamble {
        pdlpp_postamble_int(@::pack);
}  # Add genpp rule
