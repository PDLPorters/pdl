use ExtUtils::MakeMaker;

WriteMakefile(
     NAME => "PDL::IO::Dicom",
     'VERSION_FROM' => '../../Basic/Core/Version.pm',
);

# With dmake a postamble is sometimes (incorrectly) written
# in the Makefile. The following prevents that:

sub MY::postamble {
    return "";
}
