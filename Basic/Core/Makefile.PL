use strict;
use warnings;
eval { require Devel::CheckLib; Devel::CheckLib->import; };
use Config;
use ExtUtils::MakeMaker;

## $DB::single = 1;  # uncomment to have debugger stop here

my $pthread_include = $Config::Config{usrinc};  # not good for win32
my $pthread_library = '-lpthread';                                  # not good for MSVC
my $pthread_define  = ' -DPDL_PTHREAD ';

my $badval_define = " -DBADVAL=$PDL::Config{WITH_BADVAL} -DBADVAL_USENAN=$PDL::Config{BADVAL_USENAN} -DBADVAL_PER_PDL=$PDL::Config{BADVAL_PER_PDL}";

my $malloclib = $PDL::Config{MALLOCDBG}->{libs};
my $mallocinc = $PDL::Config{MALLOCDBG}->{include};

print "Trying to figure out POSIX threads support ...\n";

# TODO: replace directory and file checks for pthread.h by Devel::CheckLib test
if ( exists $PDL::Config{POSIX_THREADS_INC} and  defined $PDL::Config{POSIX_THREADS_INC} ) {
    $pthread_include = $PDL::Config{POSIX_THREADS_INC};
    print "\t..setting \$pthread_include to $pthread_include\n";
} elsif (-d $pthread_include) {
    print "\tSaw pthread.h. Fine.\n";
    $pthread_include = "-I$pthread_include"
} else {
    print "\tEhh. Didn't see include file 'pthread.h'.\n";
    $pthread_include = '';
}

# TODO: need to clean up per-platform logic herer
if ( exists $PDL::Config{POSIX_THREADS_LIBS} and defined $PDL::Config{POSIX_THREADS_LIBS} ) {
    $pthread_library = $PDL::Config{POSIX_THREADS_LIBS};
    print "\tUsing POSIX_THREADS_LIBS from perldl.conf\n";
} elsif ($Config::Config{libs} =~ /-lpthread/) {   # wrong
    print "\tFine, your perl was linked against pthread library.\n";
} elsif ($^O eq 'dec_osf') {
    if ($Config::Config{usemymalloc} eq 'n') {
        print "\tFine pthread, works with Digital Unixs malloc\n";
    } else {
        #
        print "\tPerls malloc has problems when perl is not linked with -lpthread\n";
        $pthread_library = '';
    }
} elsif ($^O eq 'freebsd'){
    if ($Config::Config{libs} =~ /-lc_r/) {
        print "\tGood, found -lc_r on a freebsd system.\n";
    } else {
        print "On FreeBSD try building perl with libc_r instead of libc\n";
        $pthread_library = '';
    }
 } elsif ($^O =~ /bsd$/i){
    if ($Config::Config{ldflags} =~ /-pthread/) {
       if ($Config::Config{usemymalloc} eq 'y') {
          print "\tGood, usemymalloc=y, will build with pthread support\n";
       } else {
          print "\tGot usemymalloc=$Config::Config{usemymalloc} so not building with pthreads\n";
          $pthread_library = '';
       }
    } else {
          print "\tMissing '-pthread' from ldflags=$Config::Config{lddlflags} so not building with pthreads\n";
          $pthread_library = '';
    }
 } else {
    print "\tNope, your perl was not linked against pthread library\n";
    if ($^O =~ /mswin/i or $^O =~ /cygwin/i) {
       if (eval { check_lib(LIB=>'-lpthread',header=>'pthread.h') }) {
          print "\tWe found -lpthread and pthread.h so will build anyway\n";
          $pthread_library = '-lpthread';
       } else {
          $pthread_library = '';
       }
    } else {
       print "\tWe'll try the default -lpthread anyway\n";
       # $pthread_library = '';
    }
 }

$pthread_include = $pthread_library = '' unless $pthread_include and $pthread_library;

{
    # TODO: use a Devel::CheckLib build/run test to verify working build params
    my $conf = $PDL::Config{WITH_POSIX_THREADS};

    if ((!defined($conf) or $conf)
	and $pthread_include and $pthread_library) {
	print "\t==> Will build PDL with POSIX thread support. Gifts to TJL :-)\n";
	$PDL::Config{WITH_POSIX_THREADS} = 1;
    } elsif($conf) {
	print "\t==> I couldn't find pthread support. However, you have\n";
	print "\t    turned on the forcing option in PDL_CONFIG so I guess I gotta do it\n";
    } else {
	print "\t==> PDL will be built without POSIX thread support.\n";
	print "\t==> *NOTE*: PDL threads are unrelated to perl threads (usethreads=y)!\n";
	print "\t==> Enabling perl threads will not help!\n";
	$pthread_define = '';
	$PDL::Config{WITH_POSIX_THREADS} = 0;
    }
}

# isbigendian() is in PDL::Dev
print "Trying to figure out endian-ness of machine...";
print " It is " . (PDL::Core::Dev::isbigendian() ? "big" : "little") . " endian\n";

sub nopl { my $txt = shift; $txt =~ s/[.]PL$//; return $txt}

# Extra targets to build

sub make_from_PL ($) {
    my $head = shift;
    return "\t" .
	'$(PERLRUNINST) ' .
	    "${head}.PL $head\n";
}

sub MY::xs_o {
    if($Config{make} =~ /\bnmake/i) {
      return'
.xs$(OBJ_EXT):
	$(PERLRUN) $(XSUBPP) $(XSPROTOARG) $(XSUBPPARGS) $*.xs > $*.c
	$(CCCMD) $(CCCDLFLAGS) "-I$(PERL_INC)" $(DEFINE) $*.c
'
      }
    else {
      package MY;
      my $self = shift;
      return $self->SUPER::xs_o;
      }
}

undef &MY::processPL; *MY::processPL = sub {
    package MY;
    my ($self) = @_;
    return $self->SUPER::processPL
        unless $^O =~ /MSWin32/i && $Config::Config{make} =~ /\b[dn]make/i;
    return '' if $Config::Config{make} =~ /\bnmake/i;
    return "" unless $self->{PL_FILES};
    my @m;
    my $alltarget = $ExtUtils::MakeMaker::VERSION >= 7.05_06 ? 'pure_nolink' : 'all';
    my $colon = $Config::Config{make} =~ /\bdmake/i ? ':' : '::';
    foreach my $plfile (sort keys %{$self->{PL_FILES}}) {
        my $list = ref($self->{PL_FILES}->{$plfile})
                ? $self->{PL_FILES}->{$plfile}
                : [$self->{PL_FILES}->{$plfile}];
        foreach my $target (@$list) {
            push @m,
                "\n$alltarget :: $target\n\t\$(NOECHO) \$(NOOP)\n",
                "\n$target $colon\n\t\$(PERLRUNINST) $plfile $target\n";
        }
    }
    join "", @m;
};

undef &MY::postamble; # suppress warning
*MY::postamble = sub {

    # nmake doesn't seem to do inference rules right so spell it out
    my $pdlsections_g = sprintf <<'EOF', PDL::Core::Dev::genpp_cmdline(qw(pdlsections.g pdlsections.c));

pdlsections.c :: pdlsections.g Types.pm
	%s
EOF
    my $text = PDL::Core::Dev::postamble() . "

pdlbadvalinit.c:: pdlbadvalinit.c.PL Types.pm\n"
. make_from_PL( 'pdlbadvalinit.c' )
."

pdldataswitch.c:: pdldataswitch.c.PL Types.pm\n"
. make_from_PL( 'pdldataswitch.c' )
."

pdl.h:: pdl.h.PL Types.pm\n"
    . make_from_PL( 'pdl.h' ) . "

pdlsimple.h:: pdlsimple.h.PL Types.pm\n"
    . make_from_PL( 'pdlsimple.h' ) . "

pdlcore.h:: pdlcore.h.PL Types.pm\n"
    . make_from_PL( 'pdlcore.h' );
if($Config{make} =~ /\bdmake/i) {
  if($ExtUtils::MakeMaker::VERSION < 7) {
    return $text . $pdlsections_g;
  } else {
#EU-MM >= 7
    return $text;
  }
} else { return
$text.
"
# Bits of C code we generate from special perl scripts
#
# there must be a more elegant way of saying that
# certain files have additional dependencies!

pdlthread.c :: pdl.h pdlcore.h\n"
."

pdlhash.c :: pdl.h pdlcore.h\n"
."

pdlapi.c :: pdl.h pdlcore.h\n"
."

pdlmagic.c :: pdlcore.h\n"
."

pdlsections.c :: pdl.h pdlcore.h\n"
."

pdlconv.c:: pdlconv.c.PL Types.pm\n"
. make_from_PL( 'pdlconv.c' )
."

pdlcore.c:: pdlcore.c.PL Types.pm\n"
. make_from_PL( 'pdlcore.c' )
.
($^O =~ /MSWin/ ? $pdlsections_g : '');
}
};

my $libs_string = "$pthread_library $malloclib -lm";

my @cfiles = qw(pdlcore pdlapi pdlhash pdlthread pdlconv pdlmagic pdlsections);
my $cobj = join ' ', map qq{$_\$(OBJ_EXT)}, @cfiles;
WriteMakefile(
 'NAME'	        => 'PDL::Core',
 'VERSION_FROM' => 'Version.pm',
  'PM'          => {
		(map {($_,'$(INST_LIBDIR)/'.$_)} (
			qw/Core.pm Basic.pm Version.pm Types.pm
						  Dbg.pm Exporter.pm Config.pm Char.pm/
		)),
		(map {($_,'$(INST_LIBDIR)/Core/'.$_)} (
			qw/Dev.pm typemap.pdl pdl.h pdlcore.h pdlmagic.h pdlsimple.h
				pdlthread.h ppport.h/
		)),
		qq/IFiles.pm/,'$(INST_LIBDIR)/Install/Files.pm',
		},
 'PL_FILES'     => {map {($_ => nopl $_)} grep {!/^Makefile.PL$/} <*.PL>},
 'OBJECT'       => 'Core$(OBJ_EXT) ' . $cobj,
 'DEFINE' 	=> $pthread_define.$badval_define,
 'LIBS'         => [$libs_string],
 'clean'        => {'FILES'  => $cobj .
                   ' pdlconv.c pdlsections.c pdlcore.c '.
		   'pdl.h pdlsimple.h pdlcore.h '.
                   'pdldataswitch.c pdlbadvalinit.c '.
		   'Types.pm Version.pm Core.c '
		   },
 'INC'          => join(' ',
   PDL_INCLUDE(), map {length($_) ? qq{"$_"} : ()} $pthread_include, $mallocinc
 ),
  depend => {
    'Core$(OBJ_EXT)' => '$(INST_ARCHLIB)$(DFSEP).exists pm_to_blib pdldataswitch.c pdlbadvalinit.c pdl.h pdlcore.h',
        # Core.xs needs blib/arch for -Mblib to work, as well as pm_to_blib
  },
 (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);
