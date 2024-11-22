use strict;
use warnings;
use ExtUtils::MakeMaker;
use PDL::Core::Dev;
use File::Spec::Functions;

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
  ($inc,$lib);
}

# these will be used in the subdirs
my ($GSL_includes, $GSL_libs) = get_gsl_libs();

my @pd_srcs;
undef &MY::init_PM; # suppress warning
*MY::init_PM = sub {
  package MY; # so that "SUPER" works right
  my ($self) = @_;
  $self->SUPER::init_PM;
  @pd_srcs = ::pdlpp_eumm_update_deep($self);
};

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pd_srcs); };

WriteMakefile(
  NAME => 'PDL::GSL',
  VERSION_FROM => 'lib/PDL/GSL/CDF.pd',
  MIN_PERL_VERSION => '5.014',
  XSMULTI => 1,
  TYPEMAPS => [PDL::Core::Dev::PDL_TYPEMAP()],
  INC => join(' ', PDL::Core::Dev::PDL_INCLUDE(), "-I".curdir(), $GSL_includes),
  LIBS => [$GSL_libs],
  clean => { FILES => join ' ', qw(MANIFEST.bak) },
  NO_MYMETA => 1,
);
