
# Makefile.PL for PDL::IO module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

use ExtUtils::MakeMaker;

# Replace this wit:e 
PDL::Core::Dev->import();

@pack = (["misc.pd",Misc,PDL::IO::Misc]);

WriteMakefile(
 pdlpp_stdargs_int(@::pack)
);

sub MY::postamble {
	pdlpp_postamble_int(@::pack);
}  # Add genpp rule


#
# Check whether FITS modules are available (external to PDL)
#
BEGIN {
  my $wstr;
  no warnings;
  eval "use Astro::FITS::Header";
  if((defined $Astro::FITS::Header::VERSION)){
    $Astro::FITS::Header::VERSION =~ m/^(\d*)/;
    if($1 < 2) {
      $wstr = "Hmmm.  You appear to have the Astro::FITS::Header module installed, which\nis good, but it's version $Astro::FITS::Header::VERSION -- which doesn't help PDL (need >= 2.0).";
    }
  } else {
    $wstr = "\nHmmm. You don't appear to have the Astro::FITS::Header module installed.\nYou'll be able to read and write FITS files anyway, but FITS support is greatly\nimproved if you install it.";
  }

  if($wstr) {
    warn "\n\nPDL::IO::Misc:\n$wstr\nYou can get the latest Astro::FITS::Header module from CPAN --\npoint your browser at http://www.cpan.org.\n\n";
  }
}
