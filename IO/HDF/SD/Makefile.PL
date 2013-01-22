#
# Makefile.PL for PDL::IO::HDF::SD module.
#
use ExtUtils::MakeMaker;
use Config;

PDL::Core::Dev->import();

$package = [ "SD.pd", SD , PDL::IO::HDF::SD ];
WriteMakefile(
    NAME => 'PDL::IO::HDF::SD',
    TYPEMAPS => [ &PDL_TYPEMAP() ], 
    OBJECT => 'SD.o ',
    PM => { 
        'SD.pm' => '$(INST_LIBDIR)/SD.pm',
    },
    INC => &PDL_INCLUDE() . " -I$hdf_include_path", 
    LIBS => [ "-L$hdf_lib_path $hdf_libs -ljpeg -lz $szip" ],
    clean => {
        FILES => 'SD.pm SD.xs SD.o SD.c',
    },
    dist => { 
        COMPRESS => 'gzip', 
        SUFFIX => 'gz'
    }, 
    (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);

sub MY::postamble { pdlpp_postamble_int($package); }
