use ExtUtils::MakeMaker;

WriteMakefile(
        'NAME' => 'PDL::Transform',
        VERSION_FROM => '../../Basic/Core/Version.pm',
        PM => { map {($_ => '$(INST_LIBDIR)/Transform/'.$_)} <*.pm> },
);

