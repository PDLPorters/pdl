use ExtUtils::MakeMaker;

WriteMakefile(
        'NAME' => 'PDL::RandVar',
        VERSION_FROM => '../../Basic/Core/Version.pm',
        PM => { map {($_ => '$(INST_LIBDIR)/RandVar/'.$_)} <*.pm> },
);

