use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME' => 'PDL::Lib::GIS',
    VERSION_FROM => '../../Basic/Core/Version.pm',
    DIR =>  [ qw/ Proj / ],
    (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);

