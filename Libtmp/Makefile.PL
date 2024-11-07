use strict;
use warnings;
use ExtUtils::MakeMaker;

undef &MY::postamble; # suppress warning
*MY::postamble = sub { '' };

WriteMakefile(
  NAME => 'PDL::Lib',
  PM => {
    'Func_demo.pm' => '$(INST_LIB)/PDL/Demos/Func_demo.pm',
    map +($_ => '$(INST_LIBDIR)/'.$_), qw(DiskCache.pm Func.pm),
  },
  NO_MYMETA => 1,
);
