use strict;
use warnings;
use ExtUtils::MakeMaker;

undef &MY::postamble; # suppress warning
*MY::postamble = sub { '' };

WriteMakefile(
  NAME => 'PDL::Lib',
  VERSION_FROM => '../Basic/PDL.pm',
  PM => { map +($_ => '$(INST_LIBDIR)/'.$_), <*.pm> },
  NO_MYMETA => 1,
);
