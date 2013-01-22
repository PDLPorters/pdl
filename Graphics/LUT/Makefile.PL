#
use ExtUtils::MakeMaker;

WriteMakefile(
 'NAME'         => 'PDL::Graphics::LUT',
 'VERSION_FROM' => '../../Basic/Core/Version.pm',
 'DIR'          => [ 'tables', 'ramps' ],
 (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);

