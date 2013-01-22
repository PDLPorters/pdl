use ExtUtils::MakeMaker;
WriteMakefile(NAME => "PDL::Opt::Simplex",
	PM => {
	 map {($_ => '$(INST_LIBDIR)/'.$_)} <*.pm>
	},
	(eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);
