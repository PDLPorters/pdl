use ExtUtils::MakeMaker;

# the real stuff happens in the subdirs

sub get_gsl_libs {
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

  return ($inc,$lib);
}

# these will be used in the subdirs
($GSL_includes, $GSL_libs)  = get_gsl_libs();

WriteMakefile(
     'NAME'	=> 'PDL::GSL',
);

sub MY::postamble {}
