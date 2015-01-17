use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
	      'NAME'  	=> 'PDL::Graphics::PGPLOT::Window',
	      'VERSION_FROM' => '../../../Basic/Core/Version.pm',
	      'INC' => '-I../../../Basic/Core/', # for ppport.h
    (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);
