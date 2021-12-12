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

my $incflags = Alien::proj->cflags;

my $ppfile = "Proj4.pd";
my $package = [$ppfile, 'Proj4', $package_name];
my %hash = pdlpp_stdargs_int($package);
$hash{VERSION_FROM} = $ppfile;
#$hash{TYPEMAPS} = [&PDL_TYPEMAP()];
$hash{LIBS} = [ Alien::proj->libs ];
$hash{INC} = PDL_INCLUDE() . " $incflags";
$hash{realclean} = { FILES => '' } unless $hash{realclean};
$hash{realclean}{FILES} .= ' _Inline';

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int( $package ); };
WriteMakefile(%hash);
