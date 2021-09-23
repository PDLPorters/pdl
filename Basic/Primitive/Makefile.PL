use strict;
use warnings;
use ExtUtils::MakeMaker;
use ExtUtils::MakeMaker::Config;

my @pack = (["primitive.pd", qw(Primitive PDL::Primitive)]);

my %hash = pdlpp_stdargs_int(@pack);
$hash{LIBS}->[0] .= ' -lm';

# If we don't do this, and Perl core is using the wrapped API, then it will
# call (say) srand48_r(), and get its random numbers from drand48_r(), but we
# will get ours from drand48(), and srand48() never gets called.
$hash{CCFLAGS} ||= $Config{ccflags};
$hash{CCFLAGS} .= ' -DPERL_REENTR_API';

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};

WriteMakefile(%hash);
