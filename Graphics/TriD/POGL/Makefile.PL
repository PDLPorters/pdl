use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'PDL::Graphics::OpenGL::Perl::OpenGL',
    VERSION_FROM        => 'OpenGL.pm',
    ABSTRACT_FROM       => 'OpenGL.pm',
    (eval($ExtUtils::MakeMaker::VERSION) >= 6.3002
      ? ('LICENSE'=> 'perl')
      : ()),
    PL_FILES            => {},
    PREREQ_PM => {
        'Test::More' => 0,
        'OpenGL' => 0.58004,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'PDL-Graphics-OpenGL-Perl-OpenGL-*' },
);
