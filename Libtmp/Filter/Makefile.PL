use strict;
use warnings;
use ExtUtils::MakeMaker;
use PDL::Core::Dev;

WriteMakefile(
  NAME => 'PDL::Filter',
  PM => { map +($_ => '$(INST_LIBDIR)/Filter/'.$_), <*.pm> },
  NO_MYMETA => 1,
);
