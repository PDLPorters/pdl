
# Makefile.PL for PDL::Graphics

use ExtUtils::MakeMaker;
# # See lib/ExtUtils/MakeMaker.pm for details of how to influence
# # the contents of the Makefile that is written.
WriteMakefile(
     'NAME'	=> 'PDL::Graphics',
     'DIR'      => ['IIS'],
     'VERSION_FROM'	=> '../Core/Core.pm',  
     'PM'       => {'PG.pm'  => '$(INST_LIBDIR)/Graphics/PG.pm'},
     'linkext'  => {LINKTYPE => ''},  # Only .pm files in this directory
);

