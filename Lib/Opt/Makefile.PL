use ExtUtils::MakeMaker;
WriteMakefile(
	'NAME' => 'PDL::Opt',
	VERSION_FROM => '../../Basic/Core/Version.pm',
	Dir => [qw/Simplex Golden/]
);

