
use lib "..";
use PDL::Core::Dev;

use ExtUtils::MakeMaker;
# # See lib/ExtUtils/MakeMaker.pm for details of how to influence
# # the contents of the Makefile that is written.
WriteMakefile(
     'NAME'	=> 'PDL',
     'VERSION_FROM'	=> 'Core/Core.pm',  
     'PM'       => {'PDL.pm'     => '$(INST_LIBDIR)/PDL.pm',
                    'PDL_OO.pm' => '$(INST_LIBDIR)/PDL/OO.pm'}, 
     'DIR'      => ['Core','Examples','Graphics','Io'],
);
