#!/usr/bin/perl
use strict;
use warnings;
use PDL;
use Inline qw{Pdlpp};

my $v = pdl(1)->pdl_core_version()->at(0);

print <<"EOPM";
package PDL::Config::Debian;
our \$pdl_core_version = $v;
1;
EOPM

__DATA__

__Pdlpp__

pp_def('pdl_core_version',
	Pars => 'dummy(); int [o] pcv();',
	Code => '$pcv() = PDL_CORE_VERSION;');

pp_done;
