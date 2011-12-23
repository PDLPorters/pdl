use ExtUtils::MakeMaker;

# "Globe/Globe.pm" puts Globe.pm in the correct subdir!
WriteMakefile(
        'NAME' => 'PDL::Transform::Cartography',
        VERSION_FROM => '../../../Basic/Core/Version.pm',
	      PM => { (map {($_ => '$(INST_LIBDIR)/'.$_)}  <*.pm>), 
		      (map {($_ => '$(INST_LIBDIR)/Cartography/'.$_)} <*.fits *.jpg>)}
);

