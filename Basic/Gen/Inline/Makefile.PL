use strict;
use ExtUtils::MakeMaker;
WriteMakefile(
	      NAME => 'Inline',
	      VERSION_FROM => 'Pdlpp.pm',
	      PREREQ_PM => {'Inline' => 0.43},
	      PM => { map {($_ => '$(INST_LIBDIR)/Inline/'.$_)} <*.pm> },
);
