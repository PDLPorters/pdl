use strict;
use warnings;
use ExtUtils::MakeMaker;

PDL::Core::Dev->import();

# do we build PDL::IO::Browser ?
my @dirs = qw( FastRaw Misc FlexRaw Pnm Storable FITS HDF GD Dicom IDL );
if ($PDL::Config{WITH_IO_BROWSER} && $PDL::Config{WITH_IO_BROWSER}) {
   push @dirs, 'Browser';
   print "\n   Building PDL::IO::Browser. Turn off WITH_IO_BROWSER if this is incorrect.\n\n";
} else {
   print "\n   Not building PDL::IO::Browser. Turn on WITH_IO_BROWSER if this is incorrect.\n\n";
}

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
