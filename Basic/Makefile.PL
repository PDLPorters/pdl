use strict;
use warnings;
use ExtUtils::MakeMaker;
use File::Spec::Functions;
eval { require Devel::CheckLib; Devel::CheckLib->import; };

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
  my $typespm = catfile(qw(lib PDL Types.pm));
  $text .= join '', map "$_ :: $typespm\n", qw(pdl.h pdlperl.h);
  $text;
};

my $pthread_include = $ENV{POSIX_THREADS_INC} // '';
my $pthread_library = $ENV{POSIX_THREADS_LIBS} // '-lpthread';
if ($pthread_library) {
    (my $libname = $pthread_library) =~ s/^-l//;
    if (!eval { check_lib(lib=>$libname,header=>'pthread.h',incpath=>$pthread_include) }) {
        print "\tDevel::CheckLib with '$libname' failed\n";
        $pthread_library = '';
    }
}
if (!$pthread_library) {
    print "PDL will be built without POSIX thread support.\n";
    print "\t==> *NOTE*: PDL threads are unrelated to perl threads (usethreads=y)!\n";
    print "\t==> Enabling perl threads will not help!\n";
}
my $pthread_define  = $pthread_library ? ' -DPDL_PTHREAD ' : '';
my $libs_string = "$pthread_library -lm";

my $hdeps = join ' ', map catfile(qw(lib PDL Core), $_), qw(pdl.h pdlperl.h pdlcore.h pdlbroadcast.h pdlmagic.h);
my @cfiles = qw(pdlcore pdlapi pdlbroadcast pdlconv pdlmagic pdlaffine pdlutil);
my $cobj = join ' ', map catfile(qw(lib PDL Core), qq{$_\$(OBJ_EXT)}), @cfiles;
push @cleanup, $cobj;
my $core_obj = catfile(qw(lib PDL), 'Core$(OBJ_EXT)');

WriteMakefile(
  NAME	=> 'PDL',
  VERSION_FROM => 'lib/PDL/Core.pm',
  DIR => [qw(
    Bad Ops Ufunc Primitive Slices Math MatrixOps
    IO-Misc IO-Pnm
    Lib-Compression Lib-FFT Lib-ImageND Lib-ImageRGB
    Lib-Transform
  )],
  EXE_FILES => \@exe_files,
  XSMULTI => 1,
  XSBUILD => {
    xs => {
      'lib/PDL/Core' => {
        INC => join(' ',
          PDL::Core::Dev::PDL_INCLUDE(),
          map {length($_) ? qq{"$_"} : ()} $pthread_include
        ),
        OBJECT => "$core_obj $cobj",
      },
    },
  },
  DEFINE => $pthread_define,
  INC => PDL::Core::Dev::PDL_INCLUDE(),
  TYPEMAPS => [PDL::Core::Dev::PDL_TYPEMAP()],
  LIBS => [$libs_string],
  depend => {
    $core_obj => 'pm_to_blib '.$hdeps,
    $cobj => $hdeps,
    (map +(
      "build_dynamic_PDL_$_" => qq{\$(INST_ARCHLIB)/auto/PDL/$_/$_.\$(DLEXT)},
      "build_static_PDL_$_" => qq{\$(INST_ARCHLIB)/auto/PDL/$_/$_\$(LIB_EXT)},
    ), qw(Core)),
  },
  clean => { FILES => join ' ',
    qw(MANIFEST.bak _Inline/ .inlinepdlpp/ .inlinewith/ .pptest/), @cleanup,
    (map catfile(qw(lib PDL Core), $_), qw(pdl.h pdlperl.h)),
  },
  NO_MYMETA => 1,
);

package MY; # so that "SUPER" works right

sub _nopl { $_[0] =~ s/[.]PL$//r }

sub init_PM {
  my ($self) = @_;
  $self->SUPER::init_PM;
  my ($pm, $plf) = @$self{qw(PM PL_FILES)};
  for my $f (grep /\.PL$/, keys %$pm) {
    $pm->{ $plf->{$f} = _nopl($f) } = _nopl(delete $pm->{$f});
  }
  delete @$pm{grep /\.c$/, keys %$pm};
}
