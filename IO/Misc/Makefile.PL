
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
  $PDL::IO::Misc::wstr = '';
  # no warnings; # pre 5.6 does not like this
  eval "use Astro::FITS::Header";
  if((defined $Astro::FITS::Header::VERSION)){
    # $Astro::FITS::Header::VERSION =~ m/^(\d*)/;
    #  dist v2.1 has CVS derived VERSION 1.12 in Header.pm
    if($Astro::FITS::Header::VERSION < 1.12) {
      $PDL::IO::Misc::wstr = << "EOW";
Hmmm. You appear to have the Astro::FITS::Header module installed, which
is good, but it's version $Astro::FITS::Header::VERSION -- 
which doesn't help PDL (need >= 1.12, i.e. distribution >= v2.1).
Using internal fallback code.
EOW
    }
  } else {
    $PDL::IO::Misc::wstr = << 'EOW';
Hmmm. You don't appear to have the Astro::FITS::Header module installed.
You'll be able to read and write FITS files anyway, but FITS support is
greatly improved if you install it.
EOW
  }
}

# collate all warning messages at the end
# where the user has a chance to see them
END {
  if($PDL::IO::Misc::wstr) {
    warn << "EOF";

* Gentle warning from PDL::IO::Misc: *

$PDL::IO::Misc::wstr
You can get the latest Astro::FITS::Header module from CPAN --
point your browser at http://www.cpan.org.

EOF
  }
}
