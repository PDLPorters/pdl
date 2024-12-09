use strict;
use warnings;
use ExtUtils::MakeMaker;
use PDL::Core::Dev;

WriteMakefile(
  NAME => 'PDL::Fit',
  PM => { map {($_ => '$(INST_LIBDIR)/Fit/'.$_)} <*.pm> },
  NO_MYMETA => 1,
);
