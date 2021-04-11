use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
  'NAME' => 'PDL::Lib::GIS',
  VERSION_FROM => '../../Basic/PDL.pm',
  DIR =>  [ qw/ Proj / ],
  NO_MYMETA => 1,
);
