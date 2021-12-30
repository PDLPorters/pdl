use strict;
use warnings;
use ExtUtils::MakeMaker;
use File::Spec::Functions;

my @pack = (["bad.pd",qw(Bad PDL::Bad)]);

my %hash = pdlpp_stdargs_int(@pack);
$hash{depend} = { 'Bad$(OBJ_EXT)' => catfile(updir, qw(Core pdlperl.h)) };

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};

WriteMakefile(%hash);
