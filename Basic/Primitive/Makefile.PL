use strict;
use warnings;
use ExtUtils::MakeMaker;
use ExtUtils::MakeMaker::Config;

my @pack = (["primitive.pd", qw(Primitive PDL::Primitive)]);

my %hash = pdlpp_stdargs_int(@pack);
$hash{LIBS}->[0] .= ' -lm';
$hash{OBJECT} .= ' xoshiro256plus$(OBJ_EXT)';

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};

WriteMakefile(%hash);
