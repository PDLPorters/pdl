# Proj4 detection process (need to rework using Devel::CheckLib)
# 
# (0) check WITH_PROJ
# (1) library names to find (libproj.so, libproj.a, proj4.dll,...)
# (2) use PROJ_LIBS if set
# (3) search lib locations:
#      '/usr/lib64',
#      '/usr/local/lib64',
#      '/lib64',
#      '/usr/lib',
#      '/usr/local/lib',
#      '/lib'
# (4) support static and dynamic library checks (confirm)
# (5) include file to find (proj_api.h), projects.h is not needed
# (6) use PROJ_INC value if set
# (6) search header file locations:
#      '/usr/include',
#      '/usr/local/include'
# (7) check that pj_init_plus() returns non-NULL for inputs
#     (a) "+proj=eqc +lon_0=0"
#     (b) "+proj=ortho +ellps=WGS84 +lon_0=-90 +lat_0=40"
#     (c) Report missing files/broken install and skip build

use strict;
use warnings;

my ($msg, $forcebuild, $config_flag, $package_name);

BEGIN {
   #
   # You probably won't need to edit anything below here (until the very end):
   #

   $msg = "";
   $forcebuild=0;  # Note: forcebuild not used
   $config_flag = 'no idea';
   $package_name = 'PDL::NoIdea';

   if (defined $PDL::Config{$config_flag} && $PDL::Config{$config_flag}==0)
   {
      $msg = "\n   Will skip build of $package_name on this system   \n";
      goto skip;
   }

   if (defined $PDL::Config{$config_flag} && $PDL::Config{$config_flag}==1)
   {
      print "\n   Will forcibly try and build $package_name on this system   \n\n";
      $forcebuild=1;
   }

   # Look for Proj4 includes/libs

   # get locations from perldl.conf, if specified there:
   # @lib_locations = @{$PDL::Config{$config_libs}} if( defined $PDL::Config{$config_libs} );
   # @inc_locations = @{$PDL::Config{$config_incs}} if( defined $PDL::Config{$config_incs} );

   # unless( defined( $gis_proj_lib_path ) )
   # {
   #    $msg .= "\tCannot find $lib_name library, $fl.\n"
   #    . "\tPlease add the correct library path to Makefile.PL or install $lib_name.\n";
   # }

   # unless( defined( $include_path ) )
   # {
   #    $msg .= "\tCannot find $lib_name header file, $fi.\n"
   #    . "\tPlease add the correct library path to Makefile.PL or install $lib_name.\n";
   # }

   # Make sure everything we wanted is found:
   #
   # unless( defined( $include_path ) && defined( $gis_proj_lib_path ) )
   # {
   #    $msg .= " Skipping build of $package_name.\n";
   # }

   # If can't find working lib+inc then override $PDL::Config
   # so Proj4 stuff will not be built
   #
   # if ($msg && $forcebuild==0)
   # {
   #    warn " Testing support libraries for $package_name:\n$msg\n";
   #    $msg =~ s/\n//g;
   #    write_dummy_make( $msg );
   #    $donot = 1;
   #    $PDL::Config{$config_flag}=0;
   # }

   # Likewise, if we cannot pj_init_plus() for a couple of the test
   # parameters then warn about a broken config for PROJ4 and skip
   # the build as well.

   # If can compile/link and run with the test parameters
   # successfully then set config to 1 so Proj4 stuff is built.

} # BEGIN...

# Need to have diagnostics re what was tested and the result
# Verbose output logs initially
# What is PDL_INCLUDE() needed for?
