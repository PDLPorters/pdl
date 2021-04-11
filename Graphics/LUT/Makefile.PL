use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
 'NAME'         => 'PDL::Graphics::LUT',
 'VERSION_FROM' => '../../Basic/PDL.pm',
 'DIR'          => [ 'tables', 'ramps' ],
 NO_MYMETA => 1,
);
