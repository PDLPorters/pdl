
# Makefile.PL for PDL::Core module

#require '../Core/Dev.pm';
PDL::Core::Dev->import();

use Config;
use IO::File;

my $pthread_include = '/usr/include/pthread.h';
my $pthread_library = '-lpthread';
my $pthread_define  = '-DPDL_PTHREAD';
my $malloclib = exists $PDL_CONFIG{MALLOCDBG}->{libs} ?
  $PDL_CONFIG{MALLOCDBG}->{libs} : '';
my $mallocinc = exists $PDL_CONFIG{MALLOCDBG}->{include} ?
  $PDL_CONFIG{MALLOCDBG}->{include} : '';

open HFILE, ">mymalloc.p" or die "couldn't open mymalloc.p";
print HFILE defined $PDL_CONFIG{MALLOCDBG}->{define} ?
  "\$mymalloc = <<'EOM';\n$PDL_CONFIG{MALLOCDBG}->{define}\nEOM\n" :
   "\$mymalloc = '';\n";
print HFILE "1;\n";
close HFILE;

print "Trying to figure out POSIX threads support ...\n";

if(-e $pthread_include) {
	print "\tSaw pthread.h. Fine.\n";
} else {
	print "\tEhh. Didn't see include file 'pthread.h'.\n";
	$pthread_include = '';
}

# For SGI, I had to link a new perl - cannot dlopen libpthread...
require Config;
if ($Config::Config{libs} =~ /-lpthread/) {
	print "\tFine, your perl was linked against pthread library.\n";
} elsif ($^O eq 'dec_osf') {
	if ($Config::Config{usemymalloc} eq 'n') {
		print "\tFine pthread, works with Digital Unixs malloc\n";
	} else {
		#
		print "\tPerls malloc has problems when perl is not linked with -lpthreads\n";
		$pthread_library = '';
	}
} elsif ($^O eq 'freebsd'){
	 if ($Config::Config{libs} =~ /-lc_r/) {
		print "\tGood, found -lc_r on a freebsd system.\n";
    }else{
	   print "On FreeBSD try building perl with libc_r instead of libc\n";
		$pthread_library = '';
    }
   	
} else {
	print "\tNope, your perl was not linked against pthread library\n";
	print "\tYou need to include -lpthread in your perl libs configuration\n";
	$pthread_library = '';
    }

$pthread_include = $pthread_library = '' unless $pthread_include and $pthread_library;

{
    my $conf = $PDL_CONFIG{WITH_POSIX_THREADS};

    if ((!defined($conf) or $conf)
	and $pthread_include and $pthread_library) {
	print "\t==> Will build PDL with POSIX thread support. Gifts to TJL :-)\n";
	$PDL_CONFIG{WITH_POSIX_THREADS} = 1;
    } elsif($conf) {
	print "\t==> I couldn't find pthread support. However, you have
\t    turned on the forcing option in PDL_CONFIG so I guess I gotta do it\n";
    } else {
	print "\t==> PDL will be built without POSIX thread support.\n";
	print "\t==> *NOTE*: PDL threads are unrelated to perl threads (usethreads=y)!\n";
	print "\t==> Enabling perl threads will not help!\n";                   
	$pthread_define = '';
	$PDL_CONFIG{WITH_POSIX_THREADS} = 0;
    }
}

# isbigendian() is in PDL::Dev
print "\nTrying to figure out endian-ness of machine...";
print " It is " . (isbigendian() ? "big" : "little") . " endian\n";

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
 'DEFINE' 	=> $pthread_define,
 'LIBS'         => ["$pthread_library $malloclib"],
 'clean'        => {'FILES'  => 'pdlcore$(OBJ_EXT) pdlapi$(OBJ_EXT) '.
                   'pdlhash$(OBJ_EXT) pdlconv$(OBJ_EXT) pdlsections$(OBJ_EXT) '.
                   'pdlconv.c pdlsections.c pdlcore.c'.
		   'pdl.h pdlsimple.h pdlcore.h '.
		   'Types.pm Version.pm Core.pm Core.c Core.xs ' .
		   'mymalloc.p badsupport.p'
		   },
 'INC'          => "$mallocinc",
);

# Extra targets to build

sub make_from_PL ($) {
    my $head = shift;
    return "\t" . 
	'$(PERL) -I$(INST_ARCHLIB) -I$(INST_LIB) -I$(PERL_ARCHLIB) -I$(PERL_LIB) ' .
	    "${head}.PL $head\n";
}

sub MY::postamble {
PDL::Core::Dev::postamble().
"
# Bits of C code we generate from special perl scripts
#
# there must be a more elegant way of saying that
# certain files have additional dependencies!

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


