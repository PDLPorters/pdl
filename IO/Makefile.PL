use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
     'NAME'	=> 'PDL::IO',
     'VERSION_FROM' => '../Basic/PDL.pm',
     'PM'       => {
                    'Dumper.pm' => '$(INST_LIB)/PDL/IO/Dumper.pm',
                    'IO.pod' => '$(INST_LIB)/PDL/IO.pod',
                   },
     NO_MYMETA => 1,
);
