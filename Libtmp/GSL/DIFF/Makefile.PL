use strict;
use warnings;
use ExtUtils::MakeMaker;

our ($GSL_includes, $GSL_libs);
my $msg = undef;
my $forcebuild=0;
my $skip = 0;

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

if (defined $PDL::Config{WITH_GSL} && $PDL::Config{WITH_GSL}==0) {
  $msg = "Will skip build of PDL::GSL::DIFF on this system\n";
  $skip = 1;
} elsif ($PDL::Config{WITH_GSL}) {
  $forcebuild=1;
}

if (($skip && !$forcebuild) ||
    !gsl_diff_links_ok($GSL_libs, $GSL_includes)) {
  warn "trying to force GSL build but link test failed\n".
    "\t -- aborting GSL build\n" if $forcebuild;
  $msg .= "GSL Libraries not found... Skipping build of PDL::GSL::DIFF.\n";
  write_dummy_make( $msg );
  return;
} else {
  print "Building PDL::GSL::DIFF. Turn off WITH_GSL if there are any problems\n";
}

my @pack = (["gsl_diff.pd", qw(DIFF PDL::GSL::DIFF)]);
my %hash = pdlpp_stdargs_int(@pack);

$hash{INC} .= " $GSL_includes";
push @{$hash{LIBS}},$GSL_libs;

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };
WriteMakefile(%hash);
