use strict;
use warnings;
use ExtUtils::MakeMaker;

my $usenan = $PDL::Config{BADVAL_USENAN};
my $bvalPerPdl = $PDL::Config{BADVAL_PER_PDL};

# print a banner to the screen
if ( $usenan ) {
    print "+ using NaN for floating-point bad value";
} else {
    print "+ using -FLT_MAX/-DBL_MAX for floating-point bad values";
}
print "+ with per-piddle bad values (EXPERIMENTAL FEATURE)"
  if $bvalPerPdl;
print "\n";

my @pack = (["bad.pd",qw(Bad PDL::Bad)]);

my %hash = pdlpp_stdargs_int(@pack);

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};

WriteMakefile(%hash);
