
use ExtUtils::MakeMaker;
 PDL::Core::Dev->import();

@pack = (["rout.pd",Rout,PDL::Graphics::TriD::Rout]);

#WriteMakefile(
# pdlpp_stdargs_int(@::pack)
#);
%hash = pdlpp_stdargs_int(@::pack);
$hash{LIBS} = ['-lm'];
WriteMakefile(%hash);

sub MY::postamble {
	pdlpp_postamble_int(@::pack);
}  # Add genpp rule
