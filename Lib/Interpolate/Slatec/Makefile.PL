use strict;
use warnings;
use ExtUtils::MakeMaker;

# do we build PDL::Interpolate::Slatec ?

my $msg = "\n Will skip build of PDL::Interpolate::Slatec\n";
my $build=0;
 
if ($^O =~ /win32/i) {
    $msg = "\n Win32 systems not yet supported: no build of PDL::Interpolate::Slatec\n";
} elsif (defined $PDL::Config{WITH_SLATEC} and $PDL::Config{WITH_SLATEC} != 0 ) {
    $build=1;
} 

if ($build==0) {
    write_dummy_make( $msg );
    return;
} else {
    print "\n Building PDL::Interpolate::Slatec.\n\n";
}                                                                            

WriteMakefile(
	      NAME => "PDL::Interpolate::Slatec",
	      VERSION_FROM => "../../../Basic/Core/Version.pm",
);
