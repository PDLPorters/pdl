
require 'Core/Dev.pm'; PDL::Core::Dev->import();

use ExtUtils::MakeMaker;


$defstartup = 'default.perldlrc';
if ($^O =~ /win32/i) {
  $defstartup = 'default.pdl';
  system("copy default.perldlrc $defstartup");
}

# # See lib/ExtUtils/MakeMaker.pm for details of how to influence
# # the contents of the Makefile that is written.
WriteMakefile(
     'NAME'	=> 'PDL',
     'VERSION_FROM' => 'Core/Version.pm',
     'PM'       => {'PDL.pm'     => '$(INST_LIBDIR)/PDL.pm',
                    'Lite.pm'    => '$(INST_LIBDIR)/PDL/Lite.pm',
                    'LiteF.pm'    => '$(INST_LIBDIR)/PDL/LiteF.pm',
                    'AutoLoader.pm'    => '$(INST_LIBDIR)/PDL/AutoLoader.pm',
                    $defstartup => '$(INST_LIBDIR)/PDL/'.$defstartup},
     'MAN3PODS' => {'AutoLoader.pm' => '$(INST_MAN3DIR)/PDL::AutoLoader.$(MAN3EXT)'},
     'DIR'      => ['Pod','Gen','Core','Ops','Primitive','Slices','Test','Math'],
);

# For the lazy people
sub MY::postamble {
PDL::Core::Dev::postamble().
q~

test_tjl :
	PERL_DL_NONLAZY=1 $(FULLPERL) -I$(INST_ARCHLIB) -I$(INST_LIB) -I$(PERL_ARCHLIB) -I$(PERL_LIB) -e 'use Test::Harness qw(&runtests $$verbose); $$verbose=$(TEST_VERBOSE); runtests @ARGV;' t/*.t

~
}
