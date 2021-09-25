use strict;
use warnings;
use ExtUtils::MakeMaker;

my @pack = (["pnm.pd", qw(Pnm PDL::IO::Pnm)]);

my %hash = pdlpp_stdargs_int(@pack);
$hash{PM}{'Pic.pm'} = '$(INST_LIBDIR)/Pic.pm';
$hash{OBJECT} .= ' get$(OBJ_EXT)';

# Add genpp rule
undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };

WriteMakefile(%hash);
