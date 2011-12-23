use ExtUtils::MakeMaker;


WriteMakefile(
	'NAME' => 'PDL::Fit',
	VERSION_FROM => '../../Basic/Core/Version.pm',
	DIR =>  [ qw/Gaussian/ ],
	PM => { map {($_ => '$(INST_LIBDIR)/Fit/'.$_)} <*.pm> },
);

