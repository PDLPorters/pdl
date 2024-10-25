use strict;
use warnings;
use ExtUtils::MakeMaker;

# NOTE:
#   we hijack m51.fits from the parent directory and 
#   install that into the demos directory
WriteMakefile(
  NAME => 'PDL::Demos',
  VERSION_FROM => '../PDL.pm',
  PM => {
    'General.pm'  => '$(INST_LIBDIR)/Demos/General.pm',
    'Demos.pm'    => '$(INST_LIBDIR)/Demos.pm',
    'BAD_demo.pm'       => '$(INST_LIBDIR)/Demos/BAD_demo.pm',
    'Sound.pm'          => '$(INST_LIBDIR)/Demos/Sound.pm',
    '../m51.fits'       => '$(INST_LIBDIR)/Demos/m51.fits',
  },
  NO_MYMETA => 1,
);
