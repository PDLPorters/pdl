use strict;
use warnings;
use ExtUtils::MakeMaker;
use File::Spec::Functions;

my $pdlroot = catfile qw(script pdl);
my @exe_files = map catfile('script', $_), qw(perldl pdldoc pptemplate);
push @exe_files, "$pdlroot$Config{exe_ext}" if $^O !~ /MSWin/;
my @cleanup = "$pdlroot$Config{exe_ext}";

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
  return '' if $^O =~ /MSWin/;
  # Here, `pdl.c` is used directly because some makes (e.g., on Solaris) do not
  # support the `$<` variable in explicit rules
<<EOT;
${pdlroot}$::Config{exe_ext} : ${pdlroot}.c
\t\$(CC) \$(CFLAGS) \$(LDFLAGS) \$(CCCDLFLAGS) ${pdlroot}.c -o \$\@
EOT
};

WriteMakefile(
  NAME	=> 'PDL',
  VERSION_FROM => 'lib/PDL.pm',
  DIR      => [qw(
    Core Bad Ops Ufunc Primitive Slices Math MatrixOps
    IO-FITS IO-FastRaw IO-FlexRaw IO-Misc IO-Pnm IO-Storable
    Lib-Compression Lib-FFT Lib-ImageND Lib-ImageRGB
    Lib-Transform
    SourceFilter Doc
  )],
  EXE_FILES => \@exe_files,
  clean => { FILES => join ' ',
    qw(MANIFEST.bak _Inline/ .inlinepdlpp/ .inlinewith/ .pptest/), @cleanup,
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
