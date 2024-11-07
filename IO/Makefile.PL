use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
     'NAME'	=> 'PDL::IO',
     'VERSION_FROM' => '../Basic/PDL.pm',
     'PM'       => {
                    'Dumper.pm' => '$(INST_LIB)/PDL/IO/Dumper.pm',
                   },
     NO_MYMETA => 1,
);
