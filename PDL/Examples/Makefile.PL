
# Makefile.PL for PDL::Examples module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

use ExtUtils::MakeMaker;

use lib '../..';
use PDL::Core::Dev;            # Pick up development utilities

WriteMakefile(
 'NAME'  	=> 'PDL::Examples',
 'VERSION_FROM'	=> 'Examples.pm',
 'TYPEMAPS'     => [PDL_TYPEMAP], 
 'OBJECT'       => 'Examples$(OBJ_EXT) pdlexamples$(OBJ_EXT)',
 'INC'          => PDL_INCLUDE,
 'LIBS'         => [''],
 'clean'        => {'FILES'  => 'pdlexamples$(OBJ_EXT) pdlexamples.c'},
);

sub MY::postamble { PDL::Core::Dev::postamble() }  # Add genpp rule
