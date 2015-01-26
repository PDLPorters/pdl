use strict;
use warnings;
use ExtUtils::MakeMaker;
use Config;
PDL::Core::Dev->import();

my ($include_path, $lib_path, $linkname);
my $ppfile = "GD.pd";
my $package_name = "PDL::IO::GD";
my $lib_name = "GD";
my @find_libs = ( 'libgd.so', 'libgd.a', 'libgd.dll.a', 'bgd.dll' );
my @find_incs = ( 'gd.h' );
my $config_flag = 'WITH_GD';
my $config_libs = 'GD_LIBS';
my $config_incs = 'GD_INC';
my @lib_locations = (
    '/usr/lib64',
    '/usr/local/lib64',
    '/lib64',
    '/usr/lib',
    '/usr/local/lib',
    '/lib',
    split(/ /, $Config{libpth}),
);
my @inc_locations = (
    '/usr/include',
    '/usr/local/include',
    $Config{usrinc},
);

my $msg = "";
my $forcebuild=0;

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

# Look for GD includes/libs

# get locations from perldl.conf, if specified there:
@lib_locations = @{$PDL::Config{$config_libs}}
    if( defined $PDL::Config{$config_libs} );
@inc_locations = @{$PDL::Config{$config_incs}}
    if( defined $PDL::Config{$config_incs} );

#
# Do the search:
#
# Look for the libs:
foreach my $libdir ( @lib_locations ) {
    my $found = 0;
    foreach my $find_lib ( @find_libs ) {
        if ( -e "$libdir/$find_lib" ) {
            $lib_path = $libdir;
            $found = 1;
            # The lib name is different on windows, so we need to adjust the LIBS, below:
            $linkname = ( $find_lib =~ /bgd.dll$/ ) ? 'bgd' : 'gd';
        }
        last if $found;
    }
    last if $found;
} # foreach $libdir...

unless( defined( $lib_path ) ) {
    $msg .= "Cannot find $lib_name library, (@find_libs).\n"
          . "Please add the correct library path to Makefile.PL or install $lib_name\n.";
}

# Look for the include files:
foreach my $incdir ( @inc_locations ) {
    foreach my $find_inc ( @find_incs ) {
        if ( -e "$incdir/$find_inc" ) {
            $include_path = $incdir;
            last;
        }
    }
}

unless( defined( $include_path ) ) {
    $msg .= "Cannot find $lib_name header files, (@find_incs).\n"
          . "Please add the correct library path to Makefile.PL or install $lib_name.\n";
}

#
# Make sure everything we wanted is found:
#
my $donot = 1;
if( defined( $include_path ) && defined( $lib_path ) ) {
    $donot = 0;
}

$donot = 0 if( $forcebuild );

if ( $donot )
{
    $msg .= "\n Skipping build of $package_name.\n";
}

skip:

if ($msg ne "" && $forcebuild==0)
{
    warn $msg . "\n";
    $msg =~ s/\n//g;
    write_dummy_make( $msg );
    $donot = 1;
    $PDL::Config{$config_flag}=0;
}
else
{
    $PDL::Config{$config_flag}=1;
    print "\n   Building $package_name. Turn off $config_flag if there are any problems\n\n";
}

return if $donot;

my $package = [$ppfile, $lib_name, $package_name];
my %hash = pdlpp_stdargs($package);
$hash{VERSION_FROM} = $ppfile;
$hash{LIBS} = ["-L$lib_path -l$linkname"];
$hash{INC} = PDL_INCLUDE() . " -I$include_path";
push( @{ $hash{TYPEMAPS} }, 'typemap' );

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int( $package ); };

WriteMakefile(%hash);
