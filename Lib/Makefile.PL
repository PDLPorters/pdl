use ExtUtils::MakeMaker;

# Note Slatec now handles f77 availability itself
# Func contains the Interpolate code

WriteMakefile(
	      'NAME' => 'PDL::Lib',
	      VERSION_FROM => '../Basic/Core/Version.pm',
	      DIR =>  [ qw/Opt ImageRGB Fit FFT Filter Image2D ImageND
			CallExt Slatec GSL GIS Transform Minuit Compression
			/ ],
    (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
#			Interpolate
);

sub MY::postamble { '' }

