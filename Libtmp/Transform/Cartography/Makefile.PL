use strict;
use warnings;
use ExtUtils::MakeMaker;

# "Globe/Globe.pm" puts Globe.pm in the correct subdir!
WriteMakefile(
  NAME => 'PDL::Transform::Cartography',
  VERSION_FROM => '../../../Basic/PDL.pm',
  PM => { (map {($_ => '$(INST_LIBDIR)/'.$_)}  <*.pm>), 
          (map {($_ => '$(INST_LIBDIR)/Cartography/'.$_)} <*.fits *.jpg>)},
  NO_MYMETA => 1,
);
