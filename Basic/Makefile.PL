use strict;
use warnings;
use ExtUtils::MakeMaker;
use File::Spec::Functions;

my $pdlroot = catfile qw(script pdl);
my @exe_files = map catfile('script', $_), qw(perldl pdldoc pptemplate);
push @exe_files, "$pdlroot$Config{exe_ext}" if $^O !~ /MSWin/;
my @cleanup = ("$pdlroot$Config{exe_ext}", catfile(qw(lib PDL Types.pm)));

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
  my $text = '';
  $text .= <<EOT if $^O !~ /MSWin/;
${pdlroot}$::Config{exe_ext} : ${pdlroot}.c
\t\$(CC) \$(CFLAGS) \$(LDFLAGS) \$(CCCDLFLAGS) ${pdlroot}.c -o \$\@
EOT
  $text;
};

WriteMakefile(
  NAME	=> 'PDL',
  VERSION_FROM => 'Core/Core.pm',
  DIR => [qw(
    Core Bad Ops Ufunc Primitive Slices Math MatrixOps
    IO-Misc IO-Pnm
    Lib-Compression Lib-FFT Lib-ImageND Lib-ImageRGB
    Lib-Transform
  )],
  EXE_FILES => \@exe_files,
  clean => { FILES => join ' ',
    qw(MANIFEST.bak _Inline/ .inlinepdlpp/ .inlinewith/ .pptest/), @cleanup,
    (map catfile(qw(lib PDL Core), $_), qw(pdl.h pdlperl.h)),
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

sub _nopl { $_[0] =~ s/[.]PL$//r }

sub init_PM {
  my ($self) = @_;
  $self->SUPER::init_PM;
  my ($pm, $plf) = @$self{qw(PM PL_FILES)};
  for my $f (grep /\.PL$/, keys %$pm) {
    $pm->{ $plf->{$f} = _nopl($f) } = _nopl(delete $pm->{$f});
  }
}
