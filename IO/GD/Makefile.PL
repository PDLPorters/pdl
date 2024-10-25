use strict;
use warnings;
use ExtUtils::MakeMaker;
use Config;
use Text::ParseWords qw(shellwords quotewords);

my ($include_path, $lib_path, $linkname);
my $ppfile = "GD.pd";
my $package_name = "PDL::IO::GD";
my $lib_name = "GD";
my @find_libs = ( 'libgd.so', 'libgd.a', 'libgd.dll.a', 'bgd.dll' );
my @find_incs = ( 'gd.h' );
my @lib_locations = grep defined, (
    $ENV{GD_LIBS},
    '/usr/lib64',
    '/usr/local/lib64',
    '/lib64',
    '/usr/lib',
    '/usr/local/lib',
    '/lib',
    ($^O eq 'MSWin32'
        ? (split($Config{path_sep}, $ENV{PATH}),
            (map {s/^-L//;$_} grep /^-L/, map {s/"//g; $_} quotewords('\s+', 1, $Config{ldflags}))
        )
        : shellwords($Config{libpth})),
);
my @inc_locations = grep defined, (
    $ENV{GD_INC},
    '/usr/include',
    '/usr/local/include',
    $Config{usrinc},
    (map {s/^-I//;$_} grep /^-I/, $^O eq 'MSWin32'
        ? (map {s/"//g; $_} quotewords('\s+', 1, $Config{cppflags}))
        : shellwords($Config{cppflags})),
);

my $msg = "";

# Look for GD includes/libs
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

# Make sure everything we wanted is found:
if (!(defined $include_path && defined $lib_path)) {
    $msg .= "Skipping build of $package_name.\n";
    write_dummy_make( $msg );
    return;
}

my $package = [$ppfile, $lib_name, $package_name];
my %hash = pdlpp_stdargs_int($package);
$hash{VERSION_FROM} = $ppfile;
$hash{DEFINE} = $ENV{GD_DEFINE};
$hash{LIBS} = [qq{-L"$lib_path" -l$linkname}];
$hash{INC} = PDL_INCLUDE() . qq{ -I"$include_path"};
push( @{ $hash{TYPEMAPS} }, 'typemap' );

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int( $package ); };

WriteMakefile(%hash);
