
# Makefile.PL for PDL::Core module

use lib '../..';
use PDL::Core::Dev;

use ExtUtils::MakeMaker;
WriteMakefile(
 'NAME'	        => 'PDL::Core',
 'VERSION_FROM'	=> 'Core.pm', 
 'OBJECT'       => 'Core$(OBJ_EXT) pdlcore$(OBJ_EXT) pdlbasicops$(OBJ_EXT) '.
                   'pdlconv$(OBJ_EXT)  pdlsections$(OBJ_EXT) '.
                   'pdlstats$(OBJ_EXT) pdlmoremaths$(OBJ_EXT)',
  'PM'           => {'Core.pm'        => '$(INST_LIBDIR)/Core.pm',
                     'Dev.pm'         => '$(INST_LIBDIR)/Core/Dev.pm',
                     'pdl.h'          => '$(INST_LIBDIR)/Core/pdl.h',
                     'pdlcore.h'      => '$(INST_LIBDIR)/Core/pdlcore.h',
                     'typemap.pdl'    => '$(INST_LIBDIR)/Core/typemap.pdl'},
 'LIBS'        => [''],
 'clean'       => {'FILES'  => 'pdlcore$(OBJ_EXT) pdlbasicops$(OBJ_EXT) '.
                   'pdlconv$(OBJ_EXT) pdlsections$(OBJ_EXT) '.
                   'pdlstats$(OBJ_EXT) pdlmoremaths$(OBJ_EXT) pdlbasicops.c '.
                   'pdlconv.c pdlsections.c pdlstats.c pdlmoremaths.c'},
);

# Extra targets to build

sub MY::postamble {
PDL::Core::Dev::postamble().
'
# Bits of C code we generate from special perl scripts

pdlbasicops.c:  mkpdlbasicops.p 
	$(PERL) mkpdlbasicops.p > pdlbasicops.c

pdlconv.c:  mkpdlconv.p 
	$(PERL) mkpdlconv.p > pdlconv.c

';
}


