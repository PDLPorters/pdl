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
  $text;
};

my @cfiles = qw(pdlcore pdlapi pdlbroadcast pdlconv pdlmagic pdlaffine pdlutil);
my $cobj = join ' ', map qq{$_\$(OBJ_EXT)}, @cfiles;
WriteMakefile(
  NAME	        => 'PDL::Core',
  VERSION_FROM => '../lib/PDL.pm',
  PM          => {
    (map +($_,'$(INST_LIBDIR)/'.$_), qw/Core.pm Types.pm/),
    (map +($_,'$(INST_LIBDIR)/Core/'.$_),
            qw/Dev.pm typemap pdl.h pdlperl.h pdlcore.h pdlmagic.h
            pdlbroadcast.h ppport.h/
    ),
  },
  PL_FILES     => {map +($_ => nopl $_), grep !/^Makefile.PL$/, 'Types.pm.PL'},
  OBJECT       => 'Core$(OBJ_EXT) ' . $cobj,
  DEFINE 	=> $pthread_define,
  LIBS         => [$libs_string],
  clean        => {'FILES'  =>
		   "pdl.h pdlperl.h Types.pm Core.c"
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
