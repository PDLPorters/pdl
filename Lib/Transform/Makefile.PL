use ExtUtils::MakeMaker;

# "Globe/Globe.pm" puts Globe.pm in the correct subdir!
WriteMakefile(
        'NAME' => 'PDL::Transform',
        VERSION_FROM => '../../Basic/Core/Version.pm',
	      DIR => ['Cartography'],
        PM => { map {($_ => '$(INST_LIBDIR)/Transform/'.$_)} <*.pm> }
);


