use strict;
use warnings;
use ExtUtils::MakeMaker;

my @pack = (["misc.pd", qw(Misc PDL::IO::Misc)]);

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };

my %hash = pdlpp_stdargs_int(@pack);
$hash{OBJECT} .= ' get$(OBJ_EXT)';
WriteMakefile(%hash);
