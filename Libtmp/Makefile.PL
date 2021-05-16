use strict;
use warnings;
use ExtUtils::MakeMaker;

# Note Slatec now handles f77 availability itself
# Func contains the Interpolate code

undef &MY::postamble; # suppress warning
*MY::postamble = sub { '' };

WriteMakefile(
  NAME => 'PDL::Lib',
  VERSION_FROM => '../Basic/PDL.pm',
  PM => { map {($_ => '$(INST_LIBDIR)/'.$_)}
    grep {
      !defined $PDL::Config{WITH_SLATEC} ||
      $PDL::Config{WITH_SLATEC}==1 ||
      !($_ eq 'Gaussian.pm')
    } <*.pm>
  },
  NO_MYMETA => 1,
);
