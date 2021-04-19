use strict;
use warnings;
use ExtUtils::MakeMaker;

our ($GSL_includes, $GSL_libs);
my $msg = undef;
my $forcebuild=0;
my $skip = 0;

if (defined $PDL::Config{WITH_GSL} && $PDL::Config{WITH_GSL}==0) {
  $msg = "Will skip build of PDL::GSL::DIFF on this system\n";
  $skip = 1;
} elsif ($PDL::Config{WITH_GSL}) {
  $forcebuild=1;
}

my @pack = (["gsl_cdf.pd", qw(CDF PDL::GSL::CDF)]);
my %hash = pdlpp_stdargs_int(@pack);

$hash{INC} .= " $GSL_includes";
push @{$hash{LIBS}},$GSL_libs;

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int(@pack); };
WriteMakefile(%hash);
