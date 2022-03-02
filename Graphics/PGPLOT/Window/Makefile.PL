use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
  NAME  	=> 'PDL::Graphics::PGPLOT::Window',
  VERSION_FROM => '../../../Basic/PDL.pm',
  INC => '-I../../../Basic/Core/', # for ppport.h
  PM => {
    'Window.pm' => '$(INST_LIBDIR)/Window.pm',
    'Demo.pm' => '$(INST_LIB)/PDL/Demos/PGPLOT_OO.pm',
  },
  NO_MYMETA => 1,
);
