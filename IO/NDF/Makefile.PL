use ExtUtils::MakeMaker;

# # See lib/ExtUtils/MakeMaker.pm for details of how to influence
# # the contents of the Makefile that is written.

WriteMakefile(
	      NAME => 'PDL::IO::NDF',
	      PM  => { 'NDF.pm' => '$(INST_LIBDIR)/NDF.pm' },
#     'VERSION_FROM' => 'NDF.pm',
	      clean => { FILES => 'NDF.pm' },
);

# try and get NDF.pm to be re-created if Basic/Core/badsupport.p is changed
#
sub MY::postamble {
"NDF.pm :: NDF.pm.PL ../../Basic/Core/badsupport.p
\t\$(PERL) -I\$(INST_ARCHLIB) -I\$(INST_LIB) -I\$(PERL_ARCHLIB) -I\$(PERL_LIB) NDF.pm.PL NDF.pm

";
}

