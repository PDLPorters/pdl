use strict;
use warnings;
use ExtUtils::MakeMaker;
WriteMakefile(
  NAME => "PDL::Opt::Simplex",
  PM => {
    'Simplex.pm' => '$(INST_LIBDIR)/Simplex.pm',
    'Demo.pm' => '$(INST_LIB)/PDL/Demos/Simplex.pm',
  },
  NO_MYMETA => 1,
);
