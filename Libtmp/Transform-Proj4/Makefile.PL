use strict;
use warnings;
use ExtUtils::MakeMaker;

my $package_name = "PDL::Transform::Proj4";
my $lib_name = "Proj4";

eval { require Alien::proj };
if ($@) {
  write_dummy_make("Will skip build of $package_name on this system - no Alien::proj");
  return;
}

my $ppfile = "proj4.pd";
my $package = [$ppfile, 'Proj4', $package_name];
my %hash = pdlpp_stdargs_int($package);
$hash{VERSION_FROM} = $ppfile;
$hash{LIBS}[0] .= ' ' . Alien::proj->libs;
$hash{INC}     .= ' ' . Alien::proj->cflags;
$hash{realclean} ||= { FILES => '' };
$hash{realclean}{FILES} .= ' _Inline';
$hash{PM}{'Demo.pm'} = '$(INST_LIB)/PDL/Demos/Proj4_demo.pm';

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int( $package ); };
WriteMakefile(%hash);
