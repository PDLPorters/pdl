use ExtUtils::MakeMaker;

# Note Slatec now handles f77 availability itself
# Func contains the Interpolate code

WriteMakefile(
	      'NAME' => 'PDL::Lib',
	      VERSION_FROM => '../Basic/Core/Version.pm',
	      DIR =>  [ qw/Opt ImageRGB Fit FFT Filter Image2D ImageND
			CallExt Slatec GSL FFTW Transform
			/ ],
#			Interpolate
);

