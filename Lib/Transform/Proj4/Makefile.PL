#
# Makefile.PL for PDL::Transform::Proj4
#
# Judd Taylor, USF IMaRS
# 18 March 2003
#

use ExtUtils::MakeMaker;
use PDL::Core::Dev;
# PDL::Core::Dev->import();

use lib '../../../inc';
use Devel::CheckLib;

BEGIN
{
   # Generic descripton of how to optionally add this package to the PDL Tree:
   $package_name = "PDL::Transform::Proj4";
   $lib_name = "Proj4";
   $find_libs = [ 'libproj.so', 'libproj.a' ];
   $find_incs = [ 'proj_api.h' ];
   $config_flag = 'WITH_PROJ';
   $config_libs = 'PROJ_LIBS';
   $config_incs = 'PROJ_INC';
   @lib_locations = (
      '/usr/lib64',
      '/usr/local/lib64',
      '/lib64',
      '/usr/lib',
      '/usr/local/lib',
      '/lib'
   );
   @inc_locations = (
      '/usr/include',
      '/usr/local/include'
   );

   #
   # You probably won't need to edit anything below here (until the very end):
   #

   $msg = "";
   $forcebuild=0;  # Note: forcebuild not used

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
   @lib_locations = @{$PDL::Config{$config_libs}}
   if( defined $PDL::Config{$config_libs} );
   @inc_locations = @{$PDL::Config{$config_incs}}
   if( defined $PDL::Config{$config_incs} );

   #
   # Do the search:
   #

   my $fl;                    #fl stores find_lib iterator for use in $msg

   foreach my $libdir ( @lib_locations )
   {
      my $found = 0;
      foreach my $find_lib ( @$find_libs )
      {
         $fl = $find_lib;
         if ( -e "$libdir/$find_lib" )
         {
            $transform_proj4_lib_path = $libdir; # use a variable name that's unique to this Makefile.PL
            print "\t$package_name: Found $libdir/$find_lib\n";
            $found = 1;
         }
         last if $found;
      }
      last if $found;
   } # foreach $libdir...

   unless( defined( $transform_proj4_lib_path ) )
   {
      $msg .= "\tCannot find $lib_name library, $fl.\n"
      . "\tPlease add the correct library path to Makefile.PL or install $lib_name.\n";
   }


   # Look for the include files:
   my $fi;
   foreach my $incdir ( @inc_locations )
   {
      foreach my $find_inc ( @$find_incs )
      {
         $fi = $find_inc;
         if ( -e "$incdir/$find_inc" )
         {
            $include_path = $incdir;
            last;
         }
      }
   }

   unless( defined( $include_path ) )
   {
      $msg .= "\tCannot find $lib_name header file, $fi.\n"
      . "\tPlease add the correct library path to Makefile.PL or install $lib_name.\n";
   }

   #
   # Make sure everything we wanted is found:
   #
   unless( defined( $include_path ) && defined( $transform_proj4_lib_path ) )
   {
      $msg .= " Skipping build of $package_name.\n";
   }

   skip:

   if ($msg && $forcebuild==0)
   {
      warn " Testing support libraries for $package_name:\n$msg\n";
      $msg =~ s/\n//g;
      write_dummy_make( $msg );
      $donot = 1;
      $PDL::Config{$config_flag}=0;
   }
   else
   {
      my $infomsg = 'check for projUV';
      my $header = 'proj_api.h';
      my $include = "#include <$header>";
      my $progbody = 'projUV puv = { 5.0, 10.0 };';
      my $libs = "-L$gis_proj_lib_path -lproj -lm";
      my $cflags = "-I$include_path";

      if (trylink($infomsg,$include,$progbody,$libs,$cflags)) {
         # now see if pj_init works
         ## print STDERR "header=>$header, LIBS=>$libs, INC=>$cflags, function=>'projPJ mypj = pj_init_plus(\"+proj=eqc +lon_0=0\"); if (! mypj) return 1; else return 0;'\n";

         if (
            check_lib(
               function=>'projPJ mypj = pj_init_plus("+proj=eqc +lon_0=0"); if (! mypj) return 1; else return 0;',
               header=>$header,
               incpath=>$include_path,
               lib=>'proj',
               libpath=>$gis_proj_lib_path,
            )) {
            print " Building $package_name. Turn off $config_flag if there are any problems\n\n";
            $PDL::Config{$config_flag}=1;
            $donot = 0;
         }
         else {
            warn " PROJ4 library found but cannot initialize projection, won't build\n";
            $PDL::Config{$config_flag}=0;
            $donot = 1;
         }
      }
      else {
         warn " Include+Library configuration for PROJ4 does not support the projUV type\n";
         $PDL::Config{$config_flag}=0;
         $donot = 1;
      }
   }

} # BEGIN...

return if $donot;  # yes, this must be return (exit will kill Makefile.PL process)

$ppfile = "Proj4.pd";
$package = [$ppfile, Proj4, $package_name];
%hash = pdlpp_stdargs($package);
$hash{VERSION_FROM} = $ppfile;
#$hash{TYPEMAPS} = [&PDL_TYPEMAP()];
$hash{LIBS} = ["-L$transform_proj4_lib_path -lproj -lm"];
$hash{INC} = PDL_INCLUDE() . " -I$include_path";
WriteMakefile(%hash);

sub MY::postamble { pdlpp_postamble_int( $package ); }
