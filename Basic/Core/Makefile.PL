
# Makefile.PL for PDL::Core module
my $libdir;

use File::Spec;

BEGIN {
   if (defined $PDL::Config{PDL_BUILD_DIR}) {
      print STDERR "Basic/Core/Makefile.PL: got root build dir $PDL::Config{PDL_BUILD_DIR}\n";
      $libdir = File::Spec->catfile($PDL::Config{PDL_BUILD_DIR},'inc');
      print STDERR "Basic/Core/Makefile.PL: adding $libdir\n";
   }
}

use lib $libdir;

use Devel::CheckLib;

#require '../Core/Dev.pm';
PDL::Core::Dev->import();

use Config;
use IO::File;

## $DB::single = 1;  # uncomment to have debugger stop here

my $pthread_include = $Config::Config{usrinc};  # not good for win32
my $pthread_library = '-lpthread';                                  # not good for MSVC
my $pthread_define  = ' -DPDL_PTHREAD ';

my $macos_braindamage_define = ($ENV{'OSTYPE'} eq 'darwin') ? " -DMACOS_MZERO_BRAINDAMAGE " : "";

my $malloclib = $PDL::Config{MALLOCDBG}->{libs};
my $mallocinc = $PDL::Config{MALLOCDBG}->{include};

my $fh = IO::File->new( "> mymalloc.p" )
  or die "couldn't open mymalloc.p";
if ( defined $PDL::Config{MALLOCDBG}->{define} ) {
    $fh->print( "\$mymalloc = <<'EOM';\n$PDL::Config{MALLOCDBG}->{define}\nEOM\n" );
} else {
    $fh->print( "\$mymalloc = '';\n" );
}
$fh->print( "1;\n" );
$fh->close;

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

# For SGI, I had to link a new perl - cannot dlopen libpthread...
require Config;

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
    }else{
        print "On FreeBSD try building perl with libc_r instead of libc\n";
        $pthread_library = '';
    }
} elsif ($^O =~ /bsd$/i){
    if ($Config::Config{usemymalloc} eq 'y') {
        print "\tGood, usemymalloc=y, will build with pthread support\n";
    }else{
        print "\tGot usemymalloc=$Config::Config{usemymalloc} so not building with pthreads\n";
        $pthread_library = '';
    }
 } else {
    print "\tNope, your perl was not linked against pthread library\n";
    if ($^O =~ /mswin/i or $^O =~ /cygwin/i) {
       if (check_lib(LIB=>'-lpthread',header=>'pthread.h')) {
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
print "\nTrying to figure out endian-ness of machine...";
print " It is " . (PDL::Core::Dev::isbigendian() ? "big" : "little") . " endian\n";

# badsupport.p is made by the top-level Makefile, but placed
# into Basic/Core, so we clean it up here
sub nopl { my $txt = shift; $txt =~ s/[.]PL$//; return $txt}

use ExtUtils::MakeMaker;
WriteMakefile(
 'NAME'	        => 'PDL::Core',
 'VERSION_FROM' => 'Version.pm',
 'OBJECT'       => 'Core$(OBJ_EXT) pdlcore$(OBJ_EXT) pdlapi$(OBJ_EXT) '.
                   'pdlhash$(OBJ_EXT) pdlthread$(OBJ_EXT) '.
                   'pdlconv$(OBJ_EXT) pdlmagic$(OBJ_EXT) pdlsections$(OBJ_EXT) ',
  'PM'          => {
  		(map {($_,'$(INST_LIBDIR)/'.$_)} (
			qw/Core.pm Basic.pm Version.pm Types.pm
						  Dbg.pm Exporter.pm Config.pm Char.pm/
		)),
  		(map {($_,'$(INST_LIBDIR)/Core/'.$_)} (
			qw/Dev.pm typemap.pdl pdl.h pdlcore.h pdlmagic.h pdlsimple.h
				pdlthread.h ppport.h/
		)),
		},
 'PL_FILES'     => {map {($_ => nopl $_)} grep {!/^Core.pm.PL|Makefile.PL$/} <*.PL>},
 'DEFINE' 	=> $pthread_define.$macos_braindamage_define,
 'LIBS'         => ["$pthread_library $malloclib"],
 'clean'        => {'FILES'  => 'pdlcore$(OBJ_EXT) pdlapi$(OBJ_EXT) '.
                   'pdlhash$(OBJ_EXT) pdlconv$(OBJ_EXT) pdlsections$(OBJ_EXT) '.
                   'pdlconv.c pdlsections.c pdlcore.c '.
		   'pdl.h pdlsimple.h pdlcore.h '.
		   'Types.pm Version.pm Core.pm Core.c Core.xs ' .
		   'mymalloc.p badsupport.p'
		   },
 'INC'          => "$pthread_include $mallocinc",
 (eval ($ExtUtils::MakeMaker::VERSION) >= 6.57_02 ? ('NO_MYMETA' => 1) : ()),
);

# Extra targets to build

sub make_from_PL ($) {
    my $head = shift;
    return "\t" . 
	'$(PERL) -I$(INST_ARCHLIB) -I$(INST_LIB) -I$(PERL_ARCHLIB) -I$(PERL_LIB) ' .
	    "${head}.PL $head\n";
}

sub MY::xs_o {
    if($Config{make} =~ /\bnmake/i) {
      return'
.xs$(OBJ_EXT):
	$(PERLRUN) $(XSUBPP) $(XSPROTOARG) $(XSUBPPARGS) $*.xs > $*.c
	$(CCCMD) $(CCCDLFLAGS) -I$(PERL_INC) $(DEFINE) $*.c
'
      }
    else {
      package MY;
      my $self = shift;
      return $self->SUPER::xs_o;
      }
}

sub MY::processPL {
    if($^O =~ /MSWin32/i && ($Config{make} =~ /\bdmake/i || $Config{make} =~ /\bnmake/i)) {
    if($Config{make} =~ /\bnmake/i){return ''}
    my($self) = shift;
    return "" unless $self->{PL_FILES};
    my(@m, $plfile);
    foreach $plfile (sort keys %{$self->{PL_FILES}}) {
        my $list = ref($self->{PL_FILES}->{$plfile})
                ? $self->{PL_FILES}->{$plfile}
                : [$self->{PL_FILES}->{$plfile}];
        my $target;
        foreach $target (@$list) {
          push @m, "
all :: $target
	\$(NOECHO) \$(NOOP)

$target :
	\$(PERLRUNINST) $plfile $target
";
          } # close foreach
    }
    join "", @m;
    }
   else {
    package MY;
    my $self = shift;
    return $self->SUPER::processPL;
    }
}

sub MY::postamble {

if($Config{make} =~ /\bdmake/i) { return

PDL::Core::Dev::postamble().
"

Core.pm :: Core.pm.PL Types.pm badsupport.p\n"
. make_from_PL('Core.pm')
."

pdl.h:: pdl.h.PL mymalloc.p Types.pm\n"
. make_from_PL( 'pdl.h' )
."

pdlsimple.h:: pdlsimple.h.PL Types.pm\n"
. make_from_PL( 'pdlsimple.h' )
."

pdlcore.h:: pdlcore.h.PL Types.pm\n"
. make_from_PL( 'pdlcore.h' )
.
q|

pdlsections.c: pdlsections.g Types.pm
       $(PERL) -e "require './getdev.pl'" pdlsections.g > pdlsections.c
|;
} else { return
PDL::Core::Dev::postamble().
"
# Bits of C code we generate from special perl scripts
#
# there must be a more elegant way of saying that
# certain files have additional dependencies!

pdlthread.c :: pdlcore.h\n"
."

pdlhash.c :: pdlcore.h\n"
."

pdlapi.c :: pdlcore.h\n"
."

pdlmagic.c :: pdlcore.h\n"
."

Core.pm :: Core.pm.PL Types.pm badsupport.p\n"
. make_from_PL('Core.pm')
."

pdl.h:: pdl.h.PL mymalloc.p Types.pm\n"
. make_from_PL( 'pdl.h' )
."

pdlsimple.h:: pdlsimple.h.PL Types.pm\n"
. make_from_PL( 'pdlsimple.h' )
."

pdlconv.c:: pdlconv.c.PL Types.pm\n"
. make_from_PL( 'pdlconv.c' )
."

pdlcore.c:: pdlcore.c.PL Types.pm\n"
. make_from_PL( 'pdlcore.c' )
."

pdlcore.h:: pdlcore.h.PL Types.pm\n"
. make_from_PL( 'pdlcore.h' )
.

    join( "", map { "\n$_ :: ${_}.PL badsupport.p\n" . make_from_PL($_) }
    qw( Core.pm Core.xs pdlcore.c ) )
.
($^O =~ /MSWin/ ? q|

pdlsections.c: pdlsections.g Types.pm
	$(PERL) -e "require './getdev.pl'" pdlsections.g > pdlsections.c
| : '');
}
}
