use ExtUtils::MakeMaker;

WriteMakefile(
	      NAME => "PDL::IO::FITS",
	      'VERSION_FROM' => '../../Basic/Core/Version.pm',
	      );

#
# Check whether FITS modules are available (external to PDL)
#
BEGIN {
    $PDL::IO::FITS::wstr = '';
    # no warnings; # pre 5.6 does not like this
    eval "use Astro::FITS::Header";
    if((defined $Astro::FITS::Header::VERSION)){
	# $Astro::FITS::Header::VERSION =~ m/^(\d*)/;
	#  dist v2.1 has CVS derived VERSION 1.12 in Header.pm
	if($Astro::FITS::Header::VERSION < 1.12) {
	    $PDL::IO::FITS::wstr = << "EOW";
Hmmm. You appear to have the Astro::FITS::Header module installed, which
is good, but it's version $Astro::FITS::Header::VERSION -- 
which doesn't help PDL (need >= 1.12, i.e. distribution >= v2.1).
Using internal fallback code.
EOW
        }
    } else {
	$PDL::IO::FITS::wstr = << 'EOW';
Hmmm. You don't appear to have the Astro::FITS::Header module installed.
You'll be able to read and write simple FITS files anyway, but FITS support is
greatly improved if you install it.
EOW
    }

} # BEGIN

# collate all warning messages at the end
# where the user has a chance to see them
END {
    if($PDL::IO::FITS::wstr) {
	warn << "EOF";

* Gentle warning from PDL::IO::FITS: *

$PDL::IO::FITS::wstr
You can get the latest Astro::FITS::Header module from CPAN --
point your browser at http://www.cpan.org.

EOF
    }
} # END

