use strict;
use warnings;
use ExtUtils::MakeMaker;
WriteMakefile(
  NAME => 'PDL::Filter',
  VERSION_FROM => '../../Basic/PDL.pm',
  PM => { map +($_ => '$(INST_LIBDIR)/Filter/'.$_), <*.pm> },
  NO_MYMETA => 1,
);
