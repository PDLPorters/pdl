use strict;
use warnings;
use ExtUtils::MakeMaker;

my $package_name = "PDL::Transform::Proj4";
my $lib_name = "Proj4";
my $config_flag = 'WITH_PROJ';
my $config_libs = 'PROJ_LIBS';
my $config_incs = 'PROJ_INC';

my $forcebuild=0;

if (defined $PDL::Config{$config_flag} && $PDL::Config{$config_flag}==0) {
  write_dummy_make("Will skip build of $package_name on this system");
  $PDL::Config{$config_flag}=0;
  return;
}

require Alien::Proj4; # runtime not compile-time so return above will work
my @inc = Alien::Proj4->default_inc;
@inc = @{$PDL::Config{$config_incs}}
  if $PDL::Config{$config_incs} and @{$PDL::Config{$config_incs}};
push @inc, File::Spec->catdir((File::Spec->updir) x 2, qw(GIS Proj include));
Alien::Proj4->import($PDL::Config{$config_libs}, \@inc);

if (defined $PDL::Config{$config_flag} && $PDL::Config{$config_flag}==1) {
  print "   Will forcibly try and build $package_name on this system\n";
  $forcebuild=1;
}

if (!$forcebuild && !Alien::Proj4->installed) {
  write_dummy_make( <<EOF );
\tCannot find $lib_name installation.
\tPlease add the correct library path to perldl.conf or install $lib_name.
Skipping build of $package_name.
EOF
  $PDL::Config{$config_flag}=0;
  return;
}

my $lib_path = Alien::Proj4->libdir;
my $incflags = Alien::Proj4->incflags;

print "Building $package_name. Turn off $config_flag if there are any problems\n";
$PDL::Config{$config_flag}=1;

my $ppfile = "Proj4.pd";
my $package = [$ppfile, 'Proj4', $package_name];
my %hash = pdlpp_stdargs($package);
$hash{VERSION_FROM} = $ppfile;
#$hash{TYPEMAPS} = [&PDL_TYPEMAP()];
$hash{LIBS} = ["-L$lib_path -lproj -lm"];
$hash{INC} = PDL_INCLUDE() . " $incflags";
$hash{realclean} = { FILES => '' } unless $hash{realclean};
$hash{realclean}{FILES} .= ' _Inline';

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int( $package ); };
WriteMakefile(%hash);
