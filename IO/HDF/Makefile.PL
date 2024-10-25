use strict;
use warnings;
use ExtUtils::MakeMaker;

# Make sure everything we wanted is found:
if (!eval { require ExtUtils::Depends; ExtUtils::Depends->new(qw(PDL::IO::HDF Alien::HDF4)) }) {
    my $msg = "Error loading Alien::HDF4: '$@'\n";
    $msg .= "Skipping build of PDL::IO::HDF.\n";
    write_dummy_make( $msg );
    return;
}

WriteMakefile(
    NAME => 'PDL::IO::HDF',
    VERSION_FROM => 'HDF.pm',
    PM => {
        'HDF.pm' => '$(INST_LIBDIR)/HDF.pm',
    },
    dist => {
        COMPRESS => 'gzip',
        SUFFIX => 'gz',
    },
    NO_MYMETA => 1,
);
