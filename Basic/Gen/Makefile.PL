use strict;
use warnings;
use ExtUtils::MakeMaker;
use ExtUtils::MakeMaker::Config;

my @pms = map {($_ => '$(INST_LIBDIR)/'.$_)} (<*.pm>, <PP/*.pm>);

undef &MY::postamble; # suppress warning

WriteMakefile(NAME => "PDL::PP",
	PM => {@pms},
	NO_MYMETA => 1,
);
