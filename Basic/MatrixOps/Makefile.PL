use ExtUtils::MakeMaker;

@pack = (["matrixops.pd",MatrixOps,PDL::MatrixOps]);
%hash = pdlpp_stdargs_int(@::pack);

PDL::Core::Dev->import();

$hash{OBJECT} = "" unless exists $hash{OBJECT};

foreach my $file (qw (blas eigens simq svd eigen complex matrix sslib)) {
    $hash{OBJECT} .= " $file\$(OBJ_EXT)";
}

$hash{LIBS}->[0] .= " -lm ";

WriteMakefile( %hash );

sub MY::postamble {
  pdlpp_postamble_int(@::pack);
}
