
# Makefile.PL for PDL::CallExt module

use ExtUtils::MakeMaker;
WriteMakefile(
 'NAME'	        => 'PDL::CallExt',
 'VERSION_FROM' => '../../Basic/Core/Version.pm',
 'INC' => '-I../../Basic/Core'

);

