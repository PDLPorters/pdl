use strict;
use warnings;
use ExtUtils::MakeMaker;

# do we build PDL::IO::Browser ?
my @dirs = qw( FastRaw Misc FlexRaw Pnm Storable FITS HDF GD Dicom IDL );
if ($PDL::Config{WITH_IO_BROWSER} && $PDL::Config{WITH_IO_BROWSER}) {
   push @dirs, 'Browser';
   print "Building PDL::IO::Browser. Turn off WITH_IO_BROWSER if this is incorrect.\n";
} else {
   print "Not building PDL::IO::Browser. Turn on WITH_IO_BROWSER if this is incorrect.\n";
}

WriteMakefile(
     'NAME'	=> 'PDL::IO',
     'VERSION_FROM' => '../Basic/PDL.pm',
     'PM'       => {
                    'Dumper.pm' => '$(INST_LIB)/PDL/IO/Dumper.pm',
                    'IO.pod' => '$(INST_LIB)/PDL/IO.pod',
                   },
     'DIR'      => [ @dirs ],
     NO_MYMETA => 1,
);
