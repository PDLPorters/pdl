use ExtUtils::MakeMaker;

WriteMakefile(
	      'NAME' => 'PDL::Transform',
	      VERSION_FROM => '../../Basic/Core/Version.pm',
	      DIR => ['Cartography'],
        PM => { map {($_ => '$(INST_LIBDIR)/'.$_)} <*.pm> }
);



