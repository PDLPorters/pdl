use strict;
use warnings;
use ExtUtils::MakeMaker;

my @dirs = qw(
  Browser Dicom FITS FastRaw FlexRaw GD HDF IDL Misc Pnm Storable
  STL
);

WriteMakefile(
     'NAME'	=> 'PDL::IO',
     'VERSION_FROM' => '../Basic/PDL.pm',
     'PM'       => {
                    'Dumper.pm' => '$(INST_LIB)/PDL/IO/Dumper.pm',
                    'IO.pod' => '$(INST_LIB)/PDL/IO.pod',
                   },
     DIR      => \@dirs,
     NO_MYMETA => 1,
);
