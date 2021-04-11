use ExtUtils::MakeMaker;
use strict;
use warnings;

# With dmake a postamble is sometimes (incorrectly) written
# in the Makefile. The following prevents that:
undef &MY::postamble; # suppress warning
*MY::postamble = sub { return ""; };

WriteMakefile(
     NAME => "PDL::IO::IDL",
     VERSION_FROM => '../../Basic/PDL.pm',
     NO_MYMETA => 1,
);
