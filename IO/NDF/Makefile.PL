use ExtUtils::MakeMaker;

# # See lib/ExtUtils/MakeMaker.pm for details of how to influence
# # the contents of the Makefile that is written.

# create NDF.pm
#  we force it to be created before the call to WriteMakefile
#  to ensure they get installed - ie NDF.pm.PL is called after
#  Makefile.PL so is not installed. Is this sensible?
#
# - this should perhaps be done in ../Makefile.PL,
#   and only done if badflag has changed or NDF.pm 
#   doesn't exist
#
##system "$^X", "mk_NDF_pm.pl" or
##    die "\nERROR: Unable to build NDF.pm\n\n" if $?;

WriteMakefile(
	      NAME => 'PDL::IO::NDF',
	      PM  => { 'NDF.pm' => '$(INST_LIBDIR)/NDF.pm' },
#     'VERSION_FROM' => 'NDF.pm',
	      clean => { FILES => 'NDF.pm' },
);

