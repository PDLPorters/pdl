use strict;
use warnings;
use ExtUtils::MakeMaker;
WriteMakefile(
        NAME => 'PDL::Filter',
        VERSION_FROM => '../../Basic/Core/Version.pm',
        PM => {
         map {($_ => '$(INST_LIBDIR)/Filter/'.$_)}
         grep {
            !defined $PDL::Config{WITH_SLATEC} ||
            $PDL::Config{WITH_SLATEC}==1 ||
            !($_ eq 'LinPred.pm')
         } <*.pm>
        },
    (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
       );
