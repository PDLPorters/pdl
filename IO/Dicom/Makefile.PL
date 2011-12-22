use ExtUtils::MakeMaker;

WriteMakefile(
     NAME => "PDL::IO::Dicom",
     'VERSION_FROM' => '../../Basic/Core/Version.pm',
    'NO_MYMETA' => 1,
);

# With dmake a postamble is sometimes (incorrectly) written
# in the Makefile. The following prevents that:

sub MY::postamble {
    return "";
}
