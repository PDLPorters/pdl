
# Makefile.PL for PDL::CallExt module
my $malloclib = exists $PDL_CONFIG{MALLOCDBG}->{libs} ?
  $PDL_CONFIG{MALLOCDBG}->{libs} : '';
my $mallocinc = exists $PDL_CONFIG{MALLOCDBG}->{include} ?
  $PDL_CONFIG{MALLOCDBG}->{include} : '';

use ExtUtils::MakeMaker;
WriteMakefile(
 'NAME'	        => 'PDL::CallExt',
 'VERSION_FROM' => '../../Basic/Core/Version.pm',
 'INC' => "-I../../Basic/Core $mallocinc",
 'LIBS' => [$malloclib],

);

