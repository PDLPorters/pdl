use strict;
use ExtUtils::MakeMaker;
WriteMakefile(
	      NAME => 'Inline::Pdlpp',
	      VERSION_FROM => 'Pdlpp.pm',
	      PREREQ_PM => {'Inline' => 0.43},
);
