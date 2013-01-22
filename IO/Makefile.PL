
PDL::Core::Dev->import();

# do we build PDL::IO::Browser ?
#
my @dirs = qw( FastRaw Misc FlexRaw Pnm Storable FITS HDF GD Dicom );
my $build_browser = $PDL::Config{WITH_IO_BROWSER} || 0;
if ( $build_browser and ($^O !~ /win32/i) ) {
   push @dirs, 'Browser';
   print "\n   Building PDL::IO::Browser. Turn off WITH_IO_BROWSER if this is incorrect.\n\n";
} else {
   print "\n   Not building PDL::IO::Browser. Turn on WITH_IO_BROWSER if this is incorrect.\n\n";
}

use ExtUtils::MakeMaker;
# # See lib/ExtUtils/MakeMaker.pm for details of how to influence
# # the contents of the Makefile that is written.

WriteMakefile(
     'NAME'	=> 'PDL::IO',
     'VERSION_FROM' => '../Basic/Core/Version.pm',
     'PM'       => {
                    'Dumper.pm' => '$(INST_LIB)/PDL/IO/Dumper.pm',
                    'IO.pod' => '$(INST_LIB)/PDL/IO.pod',
                   },
     'DIR'      => [ @dirs ],
    (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);
