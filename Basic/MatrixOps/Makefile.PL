use ExtUtils::MakeMaker;

@pack = (["matrixops.pd",MatrixOps,PDL::MatrixOps]);
%hash = pdlpp_stdargs_int(@::pack);

PDL::Core::Dev->import();

%source = qw(blas blas
	     eigens eigens
	     simq simq
	     svd svd
	     );

foreach $func(sort keys %source) {
  $file = $source{$func};
  next if $file eq 'system';
  $hash{OBJECT} .= " $file\$(OBJ_EXT)" unless $seen{$file}++;
  $hash{DEFINE} .= ' -DMY_'.uc($func);
}

$hash{LIBS}->[0] .= " -lm ";

WriteMakefile( %hash );

sub MY::postamble {
  pdlpp_postamble_int(@::pack);
}
