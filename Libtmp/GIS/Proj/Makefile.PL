use strict;
use warnings;
use ExtUtils::MakeMaker;

my $PJ_VERSION;
my $donot;
my $package_name = "PDL::GIS::Proj";
my $lib_name = "Proj";
my $config_flag = 'WITH_PROJ';
my $config_libs = 'PROJ_LIBS';
my $config_incs = 'PROJ_INC';

my $forcebuild=0;  # Note: forcebuild not used

if (defined $PDL::Config{$config_flag} && $PDL::Config{$config_flag}==0) {
  write_dummy_make("Will skip build of $package_name on this system");
  $PDL::Config{$config_flag}=0;
  return;
}

eval { require Alien::Proj4 }; # runtime not compile-time so return above will work
my @inc = eval { Alien::Proj4->default_inc };
if ($@) {
  write_dummy_make("Will skip build of $package_name on this system");
  $PDL::Config{$config_flag}=0; # we know Proj4 doesn't work
  return;
}
@inc = @{$PDL::Config{$config_incs}}
  if $PDL::Config{$config_incs} and @{$PDL::Config{$config_incs}};
push @inc, qw(include);
Alien::Proj4->import($PDL::Config{$config_libs}, \@inc);

if (defined $PDL::Config{$config_flag} && $PDL::Config{$config_flag}==1) {
  $forcebuild=1;
}

if (!$forcebuild && !Alien::Proj4->installed) {
  write_dummy_make("Cannot find $lib_name, add library path to perldl.conf. Skipping $package_name.");
  $PDL::Config{$config_flag}=0;
  return;
}

my $libflags = Alien::Proj4->libflags;
my $incflags = Alien::Proj4->incflags;

print "Building $package_name. Turn off $config_flag if there are any problems\n";
$PDL::Config{$config_flag}=1;

my $ppfile = "Proj.pd";
my $package = [$ppfile, 'Proj', $package_name];
my %hash = pdlpp_stdargs($package);
$hash{VERSION_FROM} = $ppfile;
#$hash{TYPEMAPS} = [&PDL_TYPEMAP()];
$hash{LIBS} = [ $libflags ];
$hash{INC} = PDL_INCLUDE() . " $incflags";

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int( $package ); };

WriteMakefile(%hash);
