
PDL::Core::Dev->import();

# do we build PDL::IO::Browser ?
my @dirs = qw( FastRaw Misc FlexRaw Pnm NDF Storable );
my $build_browser = $PDL_CONFIG{WITH_IO_BROWSER} || 0;
if ( $build_browser ) {
    # PDL::IO::Browser isn't (yet?) supported for win32 systems
    if ( $^O =~ /win32/i ) {
        print "\n   PDL::IO::Browser is not supported on Win32 systems - will not build.\n";
    } else {
      push @dirs, 'Browser';
      print "\n   Building PDL::IO::Browser. Turn off WITH_IO_BROWSER if this is incorrect.\n\n";
    }
} else {
    print "\n   Not building PDL::IO::Browser. Turn on WITH_IO_BROWSER if this is incorrect.\n\n";
}

use ExtUtils::MakeMaker;
# # See lib/ExtUtils/MakeMaker.pm for details of how to influence
# # the contents of the Makefile that is written.

WriteMakefile(
     'NAME'	=> 'PDL::IO',
     'VERSION_FROM' => '../Basic/Core/Version.pm',
     'PM'       => { 'Dumper.pm' => '$(INST_LIB)/PDL/IO/Dumper.pm' },
     'DIR'      => [ @dirs ],
);


