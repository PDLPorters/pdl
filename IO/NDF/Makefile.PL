use ExtUtils::MakeMaker;

# # See lib/ExtUtils/MakeMaker.pm for details of how to influence
# # the contents of the Makefile that is written.

# create NDF.pm and NDF.xs
# (we force them to be created before the call to WriteMakefile
#  to ensure they get installed - ie NDF.pm.PL is called after
#  Makefile.PL so is not installed)

foreach my $x ( "pm", "xs" ) {
    system "$^X", "mk_NDF_${x}.pl";
    die "\nERROR: Unable to build NDF.${x}\n\n" if $?;
}

WriteMakefile(
	      NAME => 'PDL::IO::NDF',
	      PM  => { 'NDF.pm' => '$(INST_LIBDIR)/NDF.pm' },
#     'VERSION_FROM' => 'NDF.pm',
              INC   => '-I../../Basic/Core',
#	      TYPEMAPS => [ '../../Graphics/PGPLOT/typemap' ],
	      clean => { FILES => 'NDF.pm NDF.xs NDF.c NDF.o' },
);

