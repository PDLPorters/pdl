
# Makefile.PL for PDL::Io module

use ExtUtils::MakeMaker;

use lib '../..';
use PDL::Core::Dev;

WriteMakefile(
 'NAME'		=> 'PDL::Io',
 'VERSION_FROM'	=> '../Core/Core.pm', 
 'TYPEMAPS'     => [PDL_TYPEMAP,'typemap'], 
 'OBJECT'    	=> 'Io$(OBJ_EXT) pdlio$(OBJ_EXT)',
 'INC'          => PDL_INCLUDE,
 'LIBS' 	=> [''],
 'clean' 	=> {'FILES'  => 'Io$(OBJ_EXT) pdlio$(OBJ_EXT)'},
);

sub MY::postamble { PDL::Core::Dev::postamble() } # Add genpp rule
