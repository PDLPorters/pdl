#
use ExtUtils::MakeMaker;

WriteMakefile(
 'NAME'         => 'PDL::Graphics::LUT',
 'VERSION_FROM' => '../../Basic/Core/Version.pm',
 'DIR'          => [ 'tables', 'ramps' ],
);

