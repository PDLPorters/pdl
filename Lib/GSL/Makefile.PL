use ExtUtils::MakeMaker;
PDL::Core::Dev->import();

# the real stuff happens in the subdirs
#
# DJB (12/30/03)
# - would it not make sense to do all the checks here and just
#   write a dummy makefile if GSL support is not available
#   (as done with some of the other modules; or is it possible/desireable
#    to compile only some of the GSL modules here?)
#
sub get_gsl_libs {
  warn << 'EOW' if ref $PDL::Config{GSL_LIBS};
 The GSL_LIBS config variable must be a string (!)
 not a reference. You should probably leave it undefined
 and rely on gsl-config. Build will likely fail.
EOW

  my $lib = ($PDL::Config{GSL_LIBS} or
	  `gsl-config --libs` or
	  warn "\tno GSL link info (libgsl probably not available)\n");
  my $inc = ($PDL::Config{GSL_INC} or
	  `gsl-config --cflags` or 
	  warn "\tno GSL include info (libgsl probably not available)\n\n");
  chomp $lib; chomp $inc;

  # print STDERR "Lib: $lib\nInc: $inc\n";

  return ($inc,$lib);
}

# these will be used in the subdirs
($GSL_includes, $GSL_libs)  = get_gsl_libs();

# Version check
my $MINVERSION = "1.3";
my $version = `gsl-config --version`;
chomp $version;
my $new_enough = 0;

if ($version =~ /^\s*$/) {
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
if (! $new_enough) {
   warn "\n   Not building GSL modules: GSL version $version found, need at least $MINVERSION\n\n";
   write_dummy_make("Not building GSL modules: GSL version $version found, but need at least $MINVERSION");
   $PDL::Config{WITH_GSL} = 0;
} elsif ( defined($PDL::Config{WITH_GSL}) and ! $PDL::Config{WITH_GSL} ) {
   warn "\n   Not building GSL modules: WITH_GSL=> 0\n\n";
   write_dummy_make("Not building GSL modules: WITH_GSL=> 0");
} else {
   $PDL::Config{WITH_GSL} = 1;
   WriteMakefile(
      'NAME'	=> 'PDL::GSL',
   );
}
sub MY::postamble {}
