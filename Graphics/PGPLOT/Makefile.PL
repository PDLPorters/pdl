use strict;
use warnings;
use ExtUtils::MakeMaker;

eval {
  local @INC = grep $_ ne '.', @INC;
  require PGPLOT;
};
if ($@) {
    write_dummy_make("Not building PDL::Graphics::PGPLOT, PGPLOT not installed");
    return;
}

WriteMakefile(
  NAME  	=> 'PDL::Graphics::PGPLOT',
  VERSION_FROM => '../../Basic/PDL.pm',
  PM => {
    'PGPLOT.pm' => '$(INST_LIBDIR)/PGPLOT.pm',
    'PGPLOTOptions.pm' => '$(INST_LIBDIR)/PGPLOTOptions.pm',
    'Demo.pm' => '$(INST_LIB)/PDL/Demos/PGPLOT.pm',
  },
  DIR => ['Window'],
  NO_MYMETA => 1,
);
