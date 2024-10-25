use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
  NAME => 'PDL::Fit',
  VERSION_FROM => '../../Basic/PDL.pm',
  PM => { map {($_ => '$(INST_LIBDIR)/Fit/'.$_)} <*.pm> },
  NO_MYMETA => 1,
);
