
# Makefile.PL for PDL::Graphics::IIS module

use ExtUtils::MakeMaker;

use lib '../../..';
use PDL::Core::Dev;

WriteMakefile(
 'NAME'   	=> 'PDL::Graphics::IIS',
 'VERSION_FROM'	=> '../../Core/Core.pm', 
 'TYPEMAPS'     => [PDL_TYPEMAP], 
 'OBJECT'    	=> 'IIS$(OBJ_EXT) pdliisdisp$(OBJ_EXT)',
 'INC'          => PDL_INCLUDE,
 'LIBS' 	=> [''],
 'clean' 	=> {'FILES'  => 'IIS.c IIS$(OBJ_EXT) pdliisdisp.c pdliisdisp$(OBJ_EXT)'},
);

sub MY::postamble { PDL::Core::Dev::postamble() } # Add genpp rule
