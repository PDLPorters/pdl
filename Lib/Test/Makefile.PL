use ExtUtils::MakeMaker;

WriteMakefile(
	'NAME' => 'Test::PDL',
	VERSION_FROM => '../../Basic/Core/Version.pm',
	PM => { 'PDL.pm' => '$(INST_LIBDIR)/../Test/PDL.pm'},
	MAN1PODS => { 'PDL.pm' => '$(INST_MAN1DIR)/Test::PDL.$(MAN1EXT)'},
);

