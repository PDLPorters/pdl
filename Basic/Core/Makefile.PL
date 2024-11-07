use strict;
use warnings;
eval { require Devel::CheckLib; Devel::CheckLib->import; };
use ExtUtils::MakeMaker::Config;
use ExtUtils::MakeMaker;

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

sub nopl { my $txt = shift; $txt =~ s/[.]PL$//; return $txt}

my $libs_string = "$pthread_library -lm";

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
  my $text = join '', map "$_ :: $_.PL Types.pm\n\t\$(PERLRUN) $_.PL $_\n",
    qw(pdl.h pdlperl.h);
  # Here, `pdl.c` is used directly because some makes (e.g., on Solaris) do not
  # support the `$<` variable in explicit rules
$text .= <<EOT if $^O !~ /MSWin/;
pdl$::Config{exe_ext} : pdl.c
\t\$(CC) \$(CFLAGS) \$(LDFLAGS) \$(CCCDLFLAGS) pdl.c -o \$\@
EOT
  $text;
};

my @exe_files = qw(perldl);
push @exe_files, "pdl$Config{exe_ext}" if $^O !~ /MSWin/;
my @cleanup = "pdl$Config{exe_ext}";

my @cfiles = qw(pdlcore pdlapi pdlbroadcast pdlconv pdlmagic pdlaffine pdlutil);
my $cobj = join ' ', map qq{$_\$(OBJ_EXT)}, @cfiles;
WriteMakefile(
  NAME	        => 'PDL::Core',
  VERSION_FROM => '../lib/PDL.pm',
  PM          => {
    (map +($_,'$(INST_LIBDIR)/'.$_),
      qw/Core.pm Basic.pm Types.pm Dbg.pm Exporter.pm Config.pm Char.pm/
    ),
    (map +($_,'$(INST_LIBDIR)/Core/'.$_),
            qw/Dev.pm typemap pdl.h pdlperl.h pdlcore.h pdlmagic.h
            pdlbroadcast.h ppport.h/
    ),
    qq/IFiles.pm/,'$(INST_LIBDIR)/Install/Files.pm',
    (map +($_=>'$(INST_LIBDIR)/Complex/'.$_), qw(Overloads.pm)),
  },
  EXE_FILES => \@exe_files,
  PL_FILES     => {map +($_ => nopl $_), grep !/^Makefile.PL$/, 'Types.pm.PL'},
  OBJECT       => 'Core$(OBJ_EXT) ' . $cobj,
  DEFINE 	=> $pthread_define,
  LIBS         => [$libs_string],
  clean        => {'FILES'  =>
		   "pdl.h pdlperl.h Types.pm Core.c @cleanup"
		   },
  INC          => join(' ',
    PDL::Core::Dev::PDL_INCLUDE(),
    map {length($_) ? qq{"$_"} : ()} $pthread_include
  ),
  depend => {
    'Core$(OBJ_EXT)' => '$(INST_ARCHLIB)$(DFSEP).exists pm_to_blib pdl.h pdlperl.h pdlcore.h',
        # Core.xs needs blib/arch for -Mblib to work, as well as pm_to_blib
    $cobj => 'pdl.h pdlcore.h pdlbroadcast.h pdlmagic.h',
    'pdlcore$(OBJ_EXT)' => 'pdlperl.h',
  },
  NO_MYMETA => 1,
);
