use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
  NAME	=> 'PDL',
  VERSION_FROM => 'lib/PDL.pm',
  DIR      => [qw(
    Gen Core Bad Ops Ufunc Primitive Slices Math MatrixOps
    IO-FITS IO-FastRaw IO-FlexRaw IO-Misc IO-Pnm IO-Storable
    Lib-Compression Lib-FFT Lib-ImageND Lib-ImageRGB
    Lib-Transform
    SourceFilter Doc
  )],
  clean => { FILES => join ' ',
    qw(MANIFEST.bak _Inline/ .inlinepdlpp/ .inlinewith/ .pptest/),
  },
  NO_MYMETA => 1,
);

# modify clean method not to delete files named 'core'
# (required for MacOSX, where "Core" and "core" are
# indistinguishable)
package MY; # so that "SUPER" works right

sub clean {
  my $inherited = shift->SUPER::clean(@_);
  $inherited =~ s/\s+core\s/ /;
  # print STDERR "processed list :\n$inherited\n";
  $inherited;
}
