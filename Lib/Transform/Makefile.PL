use ExtUtils::MakeMaker;

@pack = (["transform.pd",Transform,PDL::Transform]);
%hash = pdlpp_stdargs_int(@::pack);
$hash{DIR} = ['Cartography'];

WriteMakefile( %hash );

sub MY::postamble {
  pdlpp_postamble_int(@::pack);
}


