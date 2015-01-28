use strict;
use warnings;
use ExtUtils::MakeMaker;
PDL::Core::Dev->import();

our ($GSL_includes, $GSL_libs); 
my $msg = undef;
my $forcebuild=0;
my $skip = 0;

# this Makefile uses get_gsl_libs which is defined in
# the parent Makefile.PL

sub gsl_rng_links_ok {
  my($lib,$inc) = @_;
  return defined($lib) && defined($inc) &&
    trylink('gsl rng libraries',
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

if (defined $PDL::Config{WITH_GSL} && $PDL::Config{WITH_GSL}==0) {
  $msg = "\n   Will skip build of PDL::GSL::RNG on this system   \n";
  $skip = 1;
} elsif (defined $PDL::Config{WITH_GSL} && $PDL::Config{WITH_GSL}==1) {
  print "\n   Will forcibly try and build PDL::GSL::RNG on this system   \n\n";
  $forcebuild=1;
}

if (($skip && !$forcebuild) ||
    !gsl_rng_links_ok($GSL_libs, $GSL_includes)) {
  warn "trying to force GSL build but link test failed\n".
    "\t -- aborting GSL build\n" if $forcebuild;
  $msg ||=
    "\n GSL Libraries not found... Skipping build of PDL::GSL::RNG.\n";
  write_dummy_make( $msg );
  return;
} else {
  print "\n   Building PDL::GSL::RNG.", 
    "Turn off WITH_GSL if there are any problems\n\n";
}

my @pack = (["gsl_random.pd", qw(RNG PDL::GSL::RNG)]);
my %hash = pdlpp_stdargs_int(@pack);

$hash{INC} .= " $GSL_includes";
push @{$hash{LIBS}},$GSL_libs;

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };

WriteMakefile(%hash);
