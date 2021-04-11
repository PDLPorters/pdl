use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
	      'NAME'  	=> 'PDL::Graphics::PGPLOT::Window',
	      'VERSION_FROM' => '../../../Basic/PDL.pm',
	      'INC' => '-I../../../Basic/Core/', # for ppport.h
	      NO_MYMETA => 1,
);
