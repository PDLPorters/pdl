use ExtUtils::MakeMaker;


WriteMakefile(
	'NAME' => 'PDL::Fit',
	VERSION_FROM => '../../Basic/Core/Version.pm',
	DIR =>  [ qw/Gaussian/ ],
	PM => {'Polynomial.pm' => '$(INST_LIBDIR)/Fit/Polynomial.pm'},
);

