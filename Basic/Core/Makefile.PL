
# Makefile.PL for PDL::Core module

#require '../Core/Dev.pm';
PDL::Core::Dev->import();

my $pthread_include = '/usr/include/pthread.h';
my $pthread_library = '-lpthread';
my $pthread_define  = '-DPDL_PTHREAD';
my $malloclib = exists $PDL_CONFIG{MALLOCDBG}->{libs} ?
  $PDL_CONFIG{MALLOCDBG}->{libs} : '';
my $mallocinc = exists $PDL_CONFIG{MALLOCDBG}->{include} ?
  $PDL_CONFIG{MALLOCDBG}->{include} : '';

print "Trying to figure out POSIX threads support ...\n";

open HFILE, ">mymalloc.p" or die "couldn't open mymalloc.p";
print HFILE defined $PDL_CONFIG{MALLOCDBG}->{define} ?
  "\$mymalloc = <<'EOM';\n$PDL_CONFIG{MALLOCDBG}->{define}\nEOM\n" :
   "\$mymalloc = '';\n";
print HFILE "1;\n";
close HFILE;

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
} else {
	print "\tNope, your perl was not linked against pthread library\n";
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
	print "\t==> PDL will be built without POSIX thread support. Shame on you.\n";
	$pthread_define = '';
	$PDL_CONFIG{WITH_POSIX_THREADS} = 0;
}
}

use ExtUtils::MakeMaker;
WriteMakefile(
 'NAME'	        => 'PDL::Core',
 'VERSION_FROM' => 'Version.pm',
 'OBJECT'       => 'Core$(OBJ_EXT) pdlcore$(OBJ_EXT) pdlapi$(OBJ_EXT) '.
                   'pdlhash$(OBJ_EXT) pdlthread$(OBJ_EXT) pdlfamily$(OBJ_EXT) '.
                   'pdlconv$(OBJ_EXT)  pdlmagic$(OBJ_EXT) pdlsections$(OBJ_EXT) ',
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
 'DEFINE' 	=> $pthread_define,
 'LIBS'         => ["$pthread_library $malloclib"],
 'clean'        => {'FILES'  => 'pdlcore$(OBJ_EXT) pdlapi$(OBJ_EXT) '.
                   'pdlhash$(OBJ_EXT) pdlbasicops$(OBJ_EXT) '.
                   'pdlconv$(OBJ_EXT) pdlsections$(OBJ_EXT) '.
                   'pdlstats$(OBJ_EXT) pdlmoremaths$(OBJ_EXT) pdlbasicops.c '.
                   'pdlconv.c pdlsections.c pdlstats.c pdlmoremaths.c '.
		   'pdl.h pdlsimple.h Types.pm Version.pm'},
 'INC'          => "$mallocinc",
);

# Extra targets to build

sub MY::postamble {
PDL::Core::Dev::postamble().
'
# Bits of C code we generate from special perl scripts

pdlbasicops.c:  mkpdlbasicops.p
	$(PERL) mkpdlbasicops.p > pdlbasicops.c

pdlconv.c:  mkpdlconv.p
	$(PERL) mkpdlconv.p > pdlconv.c

'.($^O =~ /MSWin/ ? q|

pdlsections.c: pdlsections.g
       $(PERL) -e "require './getdev.pl'" pdlsections.g > pdlsections.c
| : '');
}


