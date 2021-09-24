use strict;
use warnings;
use ExtUtils::MakeMaker;

my $package_name = "PDL::Transform::Proj4";
my $lib_name = "Proj4";

eval { require Alien::Proj4 };
if ($@) {
  write_dummy_make("Will skip build of $package_name on this system - no Alien::Proj4");
  return;
}

my $incflags = Alien::Proj4->cflags;

my $ppfile = "Proj4.pd";
my $package = [$ppfile, 'Proj4', $package_name];
my %hash = pdlpp_stdargs_int($package);
$hash{VERSION_FROM} = $ppfile;
#$hash{TYPEMAPS} = [&PDL_TYPEMAP()];
$hash{LIBS} = [ Alien::Proj4->libs ];
$hash{INC} = PDL_INCLUDE() . " $incflags";
$hash{realclean} = { FILES => '' } unless $hash{realclean};
$hash{realclean}{FILES} .= ' _Inline';

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int( $package ); };
WriteMakefile(%hash);
