# PDL interface to GSL RNG and randist
# Makefile.PL for a package defined by PP code.

use ExtUtils::MakeMaker;

PDL::Core::Dev->import();

$msg = ""; $forcebuild=0;

if (defined $PDL_CONFIG{WITH_GSL} && $PDL_CONFIG{WITH_GSL}==0) {
  $msg = "\n   Will skip build of PDL::GSL on this system   \n";
  goto skip;
}

if (defined $PDL_CONFIG{WITH_GSL} && $PDL_CONFIG{WITH_GSL}==1) {
  print "\n   Will forcibly try and build PDL::GSL on this system   \n\n";
  $forcebuild=1;
}

if ($^O =~ /win32/i) {
  $msg = "\n\tWin32 systems not yet supported. Will not build PDL::GSL\n";
  goto skip unless $forcebuild;
}

$donot = 0;

# $hasgslconf = defined `gsl-config --version` ? 1 : 0;
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

# print STDERR "Lib: $lib\nInc: $inc\n";

$donot = 1 unless defined $lib && defined $inc &&
  trylink 'gsl libraries',
  << 'EOI',
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
EOI
  << 'EOB', $lib, $inc, {Hide=>1};

  const gsl_rng_type * T;
  gsl_rng * r;
  double mu = 3.0;

  T = gsl_rng_default;
  r = gsl_rng_alloc (T);
  {
      unsigned int k = gsl_ran_poisson (r, mu);
  }

EOB
if ($donot) {
  $msg =  "\n GSL Libraries not found... Skipping build of PDL::GSL.\n";
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
	  print "\n   Building PDL::GSL.", 
	  "Turn off WITH_GSL if there are any problems\n\n";
	}


return if $donot;

@pack = (["gsl_random.pd",RNG,PDL::GSL::RNG]);
%hash = pdlpp_stdargs_int(@::pack);

$hash{INC} .= " $inc";
push @{$hash{LIBS}},$lib;
WriteMakefile(%hash);


sub MY::postamble {
        pdlpp_postamble_int(@::pack);
}  # Add genpp rule                             
