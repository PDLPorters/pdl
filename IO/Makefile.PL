
PDL::Core::Dev->import();

use ExtUtils::MakeMaker;
# # See lib/ExtUtils/MakeMaker.pm for details of how to influence
# # the contents of the Makefile that is written.
WriteMakefile(
     'NAME'	=> 'PDL',
     'VERSION_FROM' => '../Basic/Core/Version.pm',
     'PM'       => {},
     'DIR'      => ['FastRaw','Misc','FlexRaw','Pnm','Browser','NDF'],
);

# For the lazy people
sub MY::postamble {
PDL::Core::Dev::postamble().
q~

test_tjl :
	PERL_DL_NONLAZY=1 $(FULLPERL) -I$(INST_ARCHLIB) -I$(INST_LIB) -I$(PERL_ARCHLIB) -I$(PERL_LIB) -e 'use Test::Harness qw(&runtests $$verbose); $$verbose=$(TEST_VERBOSE); runtests @ARGV;' t/*.t

~
}
