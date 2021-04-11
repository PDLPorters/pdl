use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
  NAME => 'PDL::Fit',
  VERSION_FROM => '../../Basic/PDL.pm',
  DIR =>  [ qw/Gaussian/ ],
  PM => { map {($_ => '$(INST_LIBDIR)/Fit/'.$_)}
  grep {
      !defined $PDL::Config{WITH_SLATEC} ||
      $PDL::Config{WITH_SLATEC}==1 ||
      !($_ eq 'Linfit.pm' || $_ eq 'LM.pm')
  } <*.pm> },
  NO_MYMETA => 1,
);
