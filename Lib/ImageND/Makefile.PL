use ExtUtils::MakeMaker;
PDL::Core::Dev->import();

@pack = (["imagend.pd",ImageND,PDL::ImageND]);

WriteMakefile( pdlpp_stdargs_int(@::pack) );

# Add genpp rule
sub MY::postamble { pdlpp_postamble_int(@::pack); }  

