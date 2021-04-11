use strict;
use warnings;
use ExtUtils::MakeMaker;

# With dmake a postamble is sometimes (incorrectly) written
# in the Makefile. The following prevents that:

undef &MY::postamble; # suppress warning
*MY::postamble = sub { return ""; };

WriteMakefile(
     NAME => "PDL::IO::Dicom",
     'VERSION_FROM' => '../../Basic/PDL.pm',
    (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);
