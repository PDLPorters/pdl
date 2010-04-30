use ExtUtils::MakeMaker;
PDL::Core::Dev->import();

@pack = (["compression.pd",Compression,PDL::Compression]);
%hash = pdlpp_stdargs_int(@pack);

$hash{OBJECT} = "" unless exists $hash{OBJECT};
$hash{OBJECT} .= "ricecomp";

WriteMakefile( pdlpp_stdargs_int(@::pack) );

# Add genpp rule
sub MY::postamble { pdlpp_postamble_int(@::pack); }  

