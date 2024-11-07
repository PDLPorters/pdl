use strict;
use warnings;
use ExtUtils::MakeMaker;

my @pods = map { $_=~s/.pod//; $_ } grep { ! m/Index.pod/ } glob("*.pod");
my @pms  = map { $_.".pod", '$(INST_LIBDIR)/' . $_ .".pod"} @pods;

WriteMakefile(
 'NAME' =>      'PDL::pod',
 'PM'    =>     { @pms },
 NO_MYMETA => 1,
);
