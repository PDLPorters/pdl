use strict;
use warnings;
use ExtUtils::MakeMaker;

my @pack = (["matrixops.pd", qw(MatrixOps PDL::MatrixOps)]);
my %hash = pdlpp_stdargs_int(@pack);

$hash{OBJECT} = "" unless exists $hash{OBJECT};

foreach my $file (qw (eigens simq svd eigen matrix sslib)) {
    $hash{OBJECT} .= " $file\$(OBJ_EXT)";
}

$hash{LIBS}->[0] .= " -lm ";

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
  pdlpp_postamble_int(@pack);
};

WriteMakefile( %hash );
