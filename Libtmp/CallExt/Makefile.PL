use strict;
use warnings;

my @cleanup = qw(t/callext$(OBJ_EXT) t/callext.$(DLEXT));

use ExtUtils::MakeMaker;
WriteMakefile(
 'NAME'	        => 'PDL::CallExt',
 'VERSION_FROM' => '../../Basic/PDL.pm',
 'INC' => "-I../../Basic/Core",
 NO_MYMETA => 1,
 clean => { FILES => join ' ', @cleanup },
);
