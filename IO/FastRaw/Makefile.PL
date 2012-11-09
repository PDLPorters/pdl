use ExtUtils::MakeMaker;

WriteMakefile(
	NAME => "PDL::IO::FastRaw",
     'VERSION_FROM' => '../../Basic/Core/Version.pm',
    (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);
