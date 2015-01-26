use strict;
use warnings;
use ExtUtils::MakeMaker;

PDL::Core::Dev->import();

my @pack = (["misc.pd", qw(Misc PDL::IO::Misc)]);

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };

WriteMakefile(
 pdlpp_stdargs_int(@pack)
);
