use strict;
use warnings;
use ExtUtils::MakeMaker;
use Config;

my $package = [ qw(VS.pd VS PDL::IO::HDF::VS) ];

undef &MY::postamble; # suppress warning
*MY::postamble = sub { pdlpp_postamble_int($package); };

my $pkg = ExtUtils::Depends->new(qw(PDL::IO::HDF::VS Alien::HDF4));
$pkg->set_inc(&PDL_INCLUDE());
$pkg->add_typemaps(&PDL_TYPEMAP());
$pkg->add_pm(
    'VS.pm' => '$(INST_LIBDIR)/VS.pm',
);

WriteMakefile(
    NAME => 'PDL::IO::HDF::VS',
    OBJECT => 'VS.o',
    $pkg->get_makefile_vars,
    clean => {
        FILES => 'VS.pm VS.xs VS.o VS.c',
    },
    dist => { 
        COMPRESS => 'gzip', 
        SUFFIX => 'gz', 
    },
    (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);
