use ExtUtils::MakeMaker;
WriteMakefile(
        NAME => 'PDL::Filter',
        VERSION_FROM => '../../Basic/Core/Version.pm',
        PM => {
         map {($_ => '$(INST_LIBDIR)/Filter/'.$_)} <*.pm>
        }
       );
__END__

