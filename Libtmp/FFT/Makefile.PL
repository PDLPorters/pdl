use strict;
use warnings;
use ExtUtils::MakeMaker;

my @pack = (["fft.pd", qw(FFT PDL::FFT)]);
my %hash = pdlpp_stdargs_int(@pack);
$hash{OBJECT} .= ' fftn$(OBJ_EXT)';
$hash{DEFINE} .= ' -DFFT_FLOAT';
$hash{LIBS} = ['-lm'] unless $^O =~ /MSWin/;

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };

WriteMakefile(%hash);
