
require 'Core/Dev.pm'; PDL::Core::Dev->import();

use ExtUtils::MakeMaker;
# # See lib/ExtUtils/MakeMaker.pm for details of how to influence
# # the contents of the Makefile that is written.
WriteMakefile(
     'NAME'	=> 'PDL::Demos',
     'VERSION_FROM' => '../Basic/Core/Version.pm',
     'PM'       => {'General.pm'     => '$(INST_LIBDIR)/Demos/General.pm',
                    'Screen.pm'    => '$(INST_LIBDIR)/Demos/Screen.pm',
                    'TriD1.pm'    => '$(INST_LIBDIR)/Demos/TriD1.pm',
                    'TriD2.pm'    => '$(INST_LIBDIR)/Demos/TriD2.pm',
                    'TriDGallery.pm'    => '$(INST_LIBDIR)/Demos/TriDGallery.pm',
		    'PGPLOT_demo.pm'	=> '$(INST_LIBDIR)/Demos/PGPLOT_demo.pm',
		   }
);

# For the lazy people
sub MY::postamble {
PDL::Core::Dev::postamble().
q~

test_tjl :
	PERL_DL_NONLAZY=1 $(FULLPERL) -I$(INST_ARCHLIB) -I$(INST_LIB) -I$(PERL_ARCHLIB) -I$(PERL_LIB) -e 'use Test::Harness qw(&runtests $$verbose); $$verbose=$(TEST_VERBOSE); runtests @ARGV;' t/*.t

~
}
