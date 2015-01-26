use strict;
use warnings;
use ExtUtils::MakeMaker;
use Config;

PDL::Core::Dev->import();

our ($hdf_include_path, $hdf_lib_path, $hdf_libs, $szip);

my $package = [ qw(VS.pd VS PDL::IO::HDF::VS) ];

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int($package); };

WriteMakefile(
    NAME => 'PDL::IO::HDF::VS',
    TYPEMAPS => [ &PDL_TYPEMAP() ],
    OBJECT => 'VS.o',
    PM => {
        'VS.pm' => '$(INST_LIBDIR)/VS.pm',
    },
    INC => &PDL_INCLUDE() . " -I$hdf_include_path",
    LIBS => [ "-L$hdf_lib_path $hdf_libs -ljpeg -lz $szip" ],
    clean => {
        FILES => 'VS.pm VS.xs VS.o VS.c',
    },
    dist => { 
        COMPRESS => 'gzip', 
        SUFFIX => 'gz', 
    },
    (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);
