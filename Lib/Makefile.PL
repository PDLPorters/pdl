use ExtUtils::MakeMaker;

# Note Slatec now handles f77 availability itself

WriteMakefile(
	'NAME' => 'PDL::Lib',
	VERSION_FROM => '../Basic/Core/Version.pm',
	DIR =>  [ qw/Opt ImageRGB Slatec FFT Filter Image2D ImageND
		     CallExt/ ],
);

