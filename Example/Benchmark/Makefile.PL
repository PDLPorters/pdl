use strict;
use warnings;

use ExtUtils::MakeMaker;
WriteMakefile(
    'NAME'	=> 'PDL::Bench',
    'VERSION_FROM' => '../../Basic/Core/Version.pm', # finds $VERSION
    'LIBS'	=> [''],   # e.g., '-lm'
    'DEFINE'	=> '',     # e.g., '-DHAVE_SOMETHING'
    'INC'	=> '',     # e.g., '-I/usr/include/other'
);
