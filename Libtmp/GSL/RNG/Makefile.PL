use strict;
use warnings;
use ExtUtils::MakeMaker;

our ($GSL_includes, $GSL_libs); 

sub gsl_rng_links_ok {
  my($lib,$inc) = @_;
  return defined($lib) && defined($inc) &&
    trylink('',
      << 'EOI',
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
EOI
	<< 'EOB', $lib, $inc);
  const gsl_rng_type * T;
  gsl_rng * r;
  double mu = 3.0;
  T = gsl_rng_default;
  r = gsl_rng_alloc (T);
  {
      unsigned int k = gsl_ran_poisson (r, mu);
  }
EOB
}

if (!gsl_rng_links_ok($GSL_libs, $GSL_includes)) {
  write_dummy_make( "Skipping build of PDL::GSL::RNG." );
  return;
}

my @pack = (["gsl_random.pd", qw(RNG PDL::GSL::RNG)]);
my %hash = pdlpp_stdargs_int(@pack);

$hash{INC} .= " $GSL_includes";
push @{$hash{LIBS}},$GSL_libs;

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };

WriteMakefile(%hash);
