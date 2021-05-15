use strict;
use warnings;
use ExtUtils::MakeMaker;

sub get_gsl_config {
  my ($flags) = @_;
  no warnings 'exec';
  `gsl-config $flags`;
}

# the real stuff happens in the subdirs
sub get_gsl_libs {
  warn << 'EOW' if ref $PDL::Config{GSL_LIBS};
 The GSL_LIBS config variable must be a string (!)
 not a reference. You should probably leave it undefined
 and rely on gsl-config. Build will likely fail.
EOW
  my $lib = ($PDL::Config{GSL_LIBS} or
	  get_gsl_config('--libs') or
	  warn "\tno GSL link info (libgsl probably not available)\n");
  my $inc = ($PDL::Config{GSL_INC} or
	  get_gsl_config('--cflags') or 
	  warn "\tno GSL include info (libgsl probably not available)\n\n");
  chomp $lib; chomp $inc;
  return ($inc,$lib);
}

# these will be used in the subdirs
our ($GSL_includes, $GSL_libs) = get_gsl_libs();

# Version check
my $MINVERSION = "1.3";
my $version = get_gsl_config('--version');
chomp $version if defined $version;
my $new_enough = 0;

if (!defined($version) or $version =~ /^\s*$/) {
  warn "\tno GSL version info found (gsl-config not installed?)\n\n";
  $version = 'UNKNOWN VERSION';
} else {
  my @is_parts =split /\./,$version;
  my @needed_parts=split /\./,$MINVERSION;
  
  $needed_parts[-1]--;
  for (my $i=0; $i<=$#needed_parts; $i++) {
    my $is_part=(exists $is_parts[$i] ? $is_parts[$i] : 0);
    $new_enough=($is_part > $needed_parts[$i]);
    last if ($new_enough);
  }
}

undef &MY::postamble; # suppress warning
*MY::postamble = sub {};
if (! $new_enough) {
   write_dummy_make("Not building GSL modules: GSL version $version found, but need at least $MINVERSION");
} else {
   WriteMakefile(
      'NAME'	=> 'PDL::GSL',
       NO_MYMETA => 1,
   );
}
