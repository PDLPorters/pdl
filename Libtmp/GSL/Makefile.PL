use strict;
use warnings;
use ExtUtils::MakeMaker;
use PDL::Core::Dev;

sub get_gsl_config {
  my ($flags) = @_;
  no warnings 'exec';
  `gsl-config $flags`;
}

# Version check
chomp (my $version = get_gsl_config('--version') // '0');
my $new_enough = 0;
if (!$version) {
  $version = 'UNKNOWN VERSION';
} else {
  my $major = (split /\./,$version)[0];
  $new_enough= $major >= 2;
}
if (!$new_enough) {
  write_dummy_make("Not building GSL modules: GSL version $version found, but need at least 2.0");
  return;
}

# the real stuff happens in the subdirs
sub get_gsl_libs {
  my $lib = ($ENV{GSL_LIBS} ||
	  get_gsl_config('--libs') ||
	  warn "\tno GSL link info (libgsl probably not available)\n");
  my $inc = ($ENV{GSL_INC} ||
	  get_gsl_config('--cflags') ||
	  warn "\tno GSL include info (libgsl probably not available)\n\n");
  chomp $lib; chomp $inc;
  return ($inc,$lib);
}

# these will be used in the subdirs
our ($GSL_includes, $GSL_libs) = get_gsl_libs();

undef &MY::postamble; # suppress warning
*MY::postamble = sub {};
WriteMakefile(
  NAME => 'PDL::GSL',
  NO_MYMETA => 1,
);
