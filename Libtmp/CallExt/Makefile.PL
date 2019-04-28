use strict;
use warnings;

my $malloclib = $PDL::Config{MALLOCDBG}->{libs};
my $mallocinc = $PDL::Config{MALLOCDBG}->{include};

my @cleanup = qw(t/callext$(OBJ_EXT) t/callext.$(DLEXT));

use ExtUtils::MakeMaker;
WriteMakefile(
 'NAME'	        => 'PDL::CallExt',
 'VERSION_FROM' => '../../Basic/PDL.pm',
 'INC' => "-I../../Basic/Core $mallocinc",
 'LIBS' => [$malloclib],
 NO_MYMETA => 1,
 clean => { FILES => join ' ', @cleanup },
);
