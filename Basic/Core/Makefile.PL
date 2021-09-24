use strict;
use warnings;
eval { require Devel::CheckLib; Devel::CheckLib->import; };
use ExtUtils::MakeMaker::Config;
use ExtUtils::MakeMaker;

my $malloclib = $PDL::Config{MALLOCDBG}->{libs};
my $mallocinc = $PDL::Config{MALLOCDBG}->{include};

my $pthread_include = '';
if ( defined $PDL::Config{POSIX_THREADS_INC} ) {
    $pthread_include = $PDL::Config{POSIX_THREADS_INC};
}
my $pthread_library = '-lpthread';
if ( defined $PDL::Config{POSIX_THREADS_LIBS} ) {
    $pthread_library = $PDL::Config{POSIX_THREADS_LIBS};
}
if ($^O eq 'freebsd' and $Config{libs} !~ /-lc_r/) {
    print "On FreeBSD try building perl with libc_r instead of libc\n";
    $pthread_library = '';
} elsif ($^O =~ /bsd$/i and $Config{usemymalloc} ne 'y') {
    print "Got usemymalloc=$Config{usemymalloc} so not building with pthreads\n";
    $pthread_library = '';
}
my $pthread_conf = $PDL::Config{WITH_POSIX_THREADS};
if (defined $pthread_conf and !$pthread_conf) {
    print "\tpthread disabled in perldl.conf\n";
    $pthread_library = '';
}
if ($pthread_library) {
    (my $libname = $pthread_library) =~ s/^-l//;
    if (!eval { check_lib(lib=>$libname,header=>'pthread.h',incpath=>$pthread_include) }) {
        print "\tDevel::CheckLib with '$libname' failed\n";
        $pthread_library = '';
    }
}
if ($pthread_library) {
    $PDL::Config{WITH_POSIX_THREADS} = 1;
} else {
    print "PDL will be built without POSIX thread support.\n";
    print "\t==> *NOTE*: PDL threads are unrelated to perl threads (usethreads=y)!\n";
    print "\t==> Enabling perl threads will not help!\n";
    $PDL::Config{WITH_POSIX_THREADS} = 0;
}
my $pthread_define  = $pthread_library ? ' -DPDL_PTHREAD ' : '';

sub nopl { my $txt = shift; $txt =~ s/[.]PL$//; return $txt}

my $libs_string = "$pthread_library $malloclib -lm";

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
    "pdlsimple.h pdl.h pdlperl.h :: Types.pm\n";
};

my @cfiles = qw(pdlcore pdlapi pdlthread pdlconv pdlmagic);
my $cobj = join ' ', map qq{$_\$(OBJ_EXT)}, @cfiles;
WriteMakefile(
 'NAME'	        => 'PDL::Core',
 'VERSION_FROM' => '../PDL.pm',
  'PM'          => {
		(map {($_,'$(INST_LIBDIR)/'.$_)} (
			qw/Core.pm Basic.pm Types.pm
			  Dbg.pm Exporter.pm Config.pm Char.pm/
		)),
		(map {($_,'$(INST_LIBDIR)/Core/'.$_)} (
			qw/Dev.pm typemap pdl.h pdlperl.h pdlcore.h pdlmagic.h pdlsimple.h
			pdlthread.h ppport.h/
		)),
		qq/IFiles.pm/,'$(INST_LIBDIR)/Install/Files.pm',
		(map +($_=>'$(INST_LIBDIR)/Complex/'.$_), qw(Overloads.pm)),
		},
 'PL_FILES'     => {map {($_ => nopl $_)} grep {!/^Makefile.PL$/} <*.PL>},
 'OBJECT'       => 'Core$(OBJ_EXT) ' . $cobj,
 'DEFINE' 	=> $pthread_define,
 'LIBS'         => [$libs_string],
 'clean'        => {'FILES'  => $cobj .
		   'pdl.h pdlperl.h pdlsimple.h '.
		   'Types.pm Core.c '
		   },
 'INC'          => join(' ',
   PDL::Core::Dev::PDL_INCLUDE(), map {length($_) ? qq{"$_"} : ()} $pthread_include, $mallocinc
 ),
  depend => {
    'Core$(OBJ_EXT)' => '$(INST_ARCHLIB)$(DFSEP).exists pm_to_blib pdl.h pdlperl.h pdlcore.h',
        # Core.xs needs blib/arch for -Mblib to work, as well as pm_to_blib
    $cobj => 'pdl.h pdlcore.h pdlthread.h pdlmagic.h',
  },
  NO_MYMETA => 1,
);
