use ExtUtils::MakeMaker;
WriteMakefile(
        NAME => 'PDL::Filter',
        VERSION_FROM => '../../Basic/Core/Version.pm',
        PM => {
         map {($_ => '$(INST_LIBDIR)/Filter/'.$_)} <*.pm>
        },
    (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
       );
__END__

