use ExtUtils::MakeMaker;
WriteMakefile(NAME => "PDL::Opt::Simplex",
	PM => {
	 map {($_ => '$(INST_LIBDIR)/'.$_)} <*.pm>
	}
);
