use strict;
use warnings;
use ExtUtils::MakeMaker;

if ($^O =~ /win32/i) {
  write_dummy_make(unsupported('PDL::Graphics::IIS','win32'));
  return;
}

my @pack = (["iis.pd", qw(IIS PDL::Graphics::IIS)]);
my %hash = pdlpp_stdargs_int(@pack);
$hash{LIBS} = ['-lm'];

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};

WriteMakefile(%hash);
