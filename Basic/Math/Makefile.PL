use strict;
use warnings;

my @pack = (["math.pd", qw(Math PDL::Math)]);
my %hash = pdlpp_stdargs_int(@pack);
$hash{OBJECT} .= $PDL::Core::Dev::EXTRAS{'math.pd'}{OBJECT};
$hash{LIBS}[0] .= " " . PDL::Core::Dev::get_maths_libs();

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};

WriteMakefile(%hash);
