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

sub gsl_interp_links_ok {
  my($lib,$inc) = @_;
  return defined($lib) && defined($inc) &&
    trylink('gsl interp libraries',
      << 'EOI',
#include <math.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_spline.h>
EOI
	<< 'EOB', $lib, $inc);

  int i;
  double xi, yi, x[10], y[10];

  for (i = 0; i < 10; i++)
    {
      x[i] = i + 0.5 * sin (i);
      y[i] = i + cos (i * i);
  }
 {
    gsl_interp_accel *acc 
      = gsl_interp_accel_alloc ();
    gsl_spline *spline 
      = gsl_spline_alloc (gsl_interp_cspline, 10);

    gsl_spline_init (spline, x, y, 10);

    yi = gsl_spline_eval (spline, x[0] + 0.01, acc);

    gsl_spline_free (spline);
    gsl_interp_accel_free(acc);
  }

EOB
}

if (defined $PDL::Config{WITH_GSL} && $PDL::Config{WITH_GSL}==0) {
  $msg = "\n   Will skip build of PDL::GSL::INTERP on this system   \n";
  $skip = 1;
} elsif (defined $PDL::Config{WITH_GSL} && $PDL::Config{WITH_GSL}==1) {
  print "\n   Will forcibly try and build PDL::GSL::INTERP on this system   \n\n";
  $forcebuild=1;
}

if (($skip && !$forcebuild) ||
    !gsl_interp_links_ok($GSL_libs, $GSL_includes)) {
  warn "trying to force GSL build but link test failed\n".
    "\t -- aborting GSL build\n" if $forcebuild;
  $msg ||=
    "\n GSL Libraries not found... Skipping build of PDL::GSL::INTERP.\n";
  write_dummy_make( $msg );
  return;
} else {
  print "\n   Building PDL::GSL::INTERP.", 
    "Turn off WITH_GSL if there are any problems\n\n";
}

my @pack = (["gsl_interp.pd", qw(INTERP PDL::GSL::INTERP)]);
my %hash = pdlpp_stdargs_int(@pack);

$hash{INC} .= " $GSL_includes";
push @{$hash{LIBS}},$GSL_libs;

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };
WriteMakefile(%hash);
