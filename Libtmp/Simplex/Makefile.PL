use strict;
use warnings;
use ExtUtils::MakeMaker;
WriteMakefile(NAME => "PDL::Opt::Simplex",
	PM => {
	 map {($_ => '$(INST_LIBDIR)/'.$_)} <*.pm>
	},
	NO_MYMETA => 1,
);
