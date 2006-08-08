#
# Makefile.PL for PDL::GIS::Proj
#
# Judd Taylor, USF IMaRS
# 18 March 2003
#

use ExtUtils::MakeMaker;
PDL::Core::Dev->import();

BEGIN
{
    # Generic descripton of how to optionally add this package to the PDL Tree:
    $package_name = "PDL::GIS::Proj";
    $lib_name = "Proj";
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
    # Look for the libs:
    local($lib_path) = undef;  #local because I'm not sure what lib_path is used for elsewhere --CD
    my $fl;                    #fl stores find_lib iterator for use in $msg

    foreach my $libdir ( @lib_locations ) 
    {
        my $found = 0;
        foreach my $find_lib ( @$find_libs )
        {
            $fl = $find_lib;
            if ( -e "$libdir/$find_lib" ) 
            {
                $lib_path = $libdir;
                print "\t$package_name: Found $libdir/$find_lib\n";
                $found = 1;
            }
            last if $found;
        }
        last if $found;
    } # foreach $libdir...

    unless( defined( $lib_path ) )
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
    unless( defined( $include_path ) && defined( $lib_path ) )
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
    } 
    else 
    {
        print " Building $package_name. Turn off $config_flag if there are any problems\n\n";
        $donot = 0;
    }

} # BEGIN...

return if $donot;

$ppfile = "Proj.pd";
$package = [$ppfile, Proj, $package_name];
%hash = pdlpp_stdargs($package);
$hash{VERSION_FROM} = $ppfile;
#$hash{TYPEMAPS} = [&PDL_TYPEMAP()];
$hash{LIBS} = ["-L$lib_path -lproj -lm"];
$hash{INC} = PDL_INCLUDE() . " -I$include_path";
WriteMakefile(%hash);

sub MY::postamble { pdlpp_postamble_int( $package ); }
