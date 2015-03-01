use strict;
use warnings;
use ExtUtils::MakeMaker;

my $msg = "";
my $forcebuild=0;

if (defined $PDL::Config{WITH_HDF} && $PDL::Config{WITH_HDF}==0) {
    $msg = "Will skip build of PDL::IO::HDF on this system\n";
    goto skip;
}

if (defined $PDL::Config{WITH_HDF} && $PDL::Config{WITH_HDF}==1) {
    print "Will forcibly try and build PDL::IO::HDF on this system\n";
    $forcebuild=1;
}

# Make sure everything we wanted is found:
my $donot = 1;
if( eval { require ExtUtils::Depends; ExtUtils::Depends->new(qw(PDL::IO::HDF4 Alien::HDF4)) } ) {
    $donot = 0;
} else {
    $msg .= "Error loading Alien::HDF4: '$@'\n";
}

if ( $donot ) {
    $msg .= "Skipping build of PDL::IO::HDF.\n";
}

skip:

if ($msg ne "" && $forcebuild==0) {
    write_dummy_make( $msg );
    $donot = 1;
    $PDL::Config{WITH_HDF}=0;
} else {
    $PDL::Config{WITH_HDF}=1;
    print "Building PDL::IO::HDF. Turn off WITH_HDF if there are any problems\n";
}

return if $donot;

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
    (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);
