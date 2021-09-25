use strict;
use warnings;
use ExtUtils::MakeMaker;
use ExtUtils::MakeMaker::Config;

if ($^O =~ /win32/i) {
  write_dummy_make(unsupported('PDL::Graphics::IIS','win32'));
  return;
}

my @pack = (["iis.pd", qw(IIS PDL::Graphics::IIS)]);
my %hash = pdlpp_stdargs_int(@pack);
$hash{LIBS} = ['-lm'];
$hash{OBJECT} .= ' pdliisdisp$(OBJ_EXT)';
$hash{INC} .= qq{ -DO_NONBLOCK="$Config{o_nonblock}"}
  if ($Config{o_nonblock}//'O_NONBLOCK') ne 'O_NONBLOCK';

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};

WriteMakefile(%hash);
