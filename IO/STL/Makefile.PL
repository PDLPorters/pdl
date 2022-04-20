use ExtUtils::MakeMaker;
use strict;
use warnings;

WriteMakefile(
  NAME => "PDL::IO::STL",
  VERSION_FROM => '../../Basic/PDL.pm',
  PM => { (map {($_ => '$(INST_LIBDIR)/'.$_)} <*.pm>),
          (map {($_ => '$(INST_LIBDIR)/STL/'.$_)} <*.stl>)},
  NO_MYMETA => 1,
);
