# Makefile.PL for a package defined by PP code.

use ExtUtils::MakeMaker;
use Config;

#eval {
#  require PDL::Core::Dev;  # needed for stand-alone version of package.
#                           # Need to comment out
#                           # for package included in PDL distro.
#
#  PDL::Core::Dev->import;
#};
#my $pdl_installed = ($@) ? 0 : 1; # make always == 1 for install from PDL distro.
my $pdl_installed = 1;
#print "PDL ", ($pdl_installed ? '' : 'not'), " installed\n";

if (!$pdl_installed) {
  # write dummy makefile if PDL not installed
  WriteMakefile(
		'NAME'         => 'PDL::Graphics::PLplot',
		'PREREQ_PM'    => { PDL => 0 },
		);
  exit;
}

use vars qw /$plplotvers/; # version of plplot used

# 
## Search for plplot library and include file
#
print "\nChecking PDL::Graphics::PLplot...\n";

unlink ("OPTIONS!"); # remove file uses to communicate with plplot.pd

# on OS-X the library is called libplplotd.dylib, so we
# use $Config{"so"} to get the extension name
#
my $libname = "libplplotd." . $Config{"so"};
my $libname_static = "libplplotd.a";
my $incname = "plplot.h";


# require bad value support.  D. Hunt 10/20/2008
unless ($PDL::Config{WITH_BADVAL}) {
  buildDummy ("Bad value support required!  (Turn on WITH_BADVAL in perldl.conf)");
  return if (caller); exit;
}

my $found_plplot =
    (defined($PDL::Config{WITH_PLPLOT}) && (!$PDL::Config{WITH_PLPLOT}))
    ? 0 : 1;
print "found_plplot is $found_plplot\n";
unless ($found_plplot) {
  buildDummy ("User requested no PLplot interface!");
  return if (caller); exit;
}
foreach my $libdir (
		    '/usr/local/plplot/lib64',
                    '/usr/local/plplot/lib',
		    '/usr/local/lib64',
		    '/usr/local/lib',
		    '/opt/local/lib',
		    '/usr/lib64',
		    '/usr/lib',
		    '/opt/lib64',
		    '/opt/lib',
		    '/ops/tools/lib',
                    '/sw/lib/',    # default FINK installation
		    $PDL::Config{WHERE_PLPLOT_LIBS},
		                  # Add new library paths here!!
		    ) {

  if (-e "$libdir/$libname") {
    $plplot_lib_path = $libdir;
    $ENV{LD_LIBRARY_PATH} .= ":$libdir";
    last;
  } elsif (-e "$libdir/$libname_static") {
    $plplot_lib_path = $libdir;
    $ENV{LD_LIBRARY_PATH} .= ":$libdir";
    $libname = $libname_static;
    last;
  }

}

unless (defined ($plplot_lib_path)) {
  buildDummy ("Cannot find plplot library ($libname), skipping PDL::Graphics::PLplot\n");
  return if (caller); exit;
}

foreach my $incdir (
                    '/usr/local/plplot/include',
                    '/usr/local/plplot/include/plplot',
		    '/usr/local/include/plplot',
		    '/usr/local/include',
		    '/opt/local/include',
		    '/usr/include',
		    '/usr/include/plplot',
		    '/opt/include',
		    '/ops/tools/include/plplot',
		    '/ops/tools/include',
                    '/sw/include/plplot/',    # default FINK installation
		    $PDL::Config{WHERE_PLPLOT_INCLUDE},
		                  # Add new header paths here!!
		    ) {

  if (-e "$incdir/$incname") {
    $plplot_include_path = "-I$incdir";
    if ($incdir =~ m|/plplot$|) {
      $inc2 = $incdir;
      $inc2 =~ s|/plplot$||;
      $plplot_include_path .= " -I$inc2";
    }
    last;
  }
}

unless (defined ($plplot_include_path)) {
  buildDummy ("Cannot find plplot header file ($incname), skipping PDL::Graphics::PLplot\n");
  return if (caller); exit;
}

# Test that PLplot was compiled --with-double (IE, a PLFLT is a double)
my $size = compileAndRun ("$plplot_include_path", 1, <<'EOC');
#include <plplot.h>
main () {  printf  ("%d", sizeof (PLFLT)); }
EOC

unless ($size == 8) {
  print "          Sizeof(PLFLT) must be 8. PLplot must be compiled --with-double (IE ./configure --with-double)\n";
  $found_plplot = 0;
} 

# Try linking with library.  New if blocks should be added if necessary
my $libs;
foreach my $l (
	       "-L$plplot_lib_path -lplplotd -lX11",
	       "-L$plplot_lib_path -lplplotd",
	       # New lib combinations should be added here!
	       ) {
  if (trylink( '', '#include <plplot.h>', 'plinit();', "$l $plplot_include_path")) {
    $libs = $l;
    print "\tSucceeded linking PLplot with $libs\n";
    last;
  } 
}

unless (defined($libs)) {
  buildDummy ("Cannot find good set of libraries for linking PLplot, try fiddling perldl.conf\n");
  return if (caller); exit;
}

# Test if PLplot has plsmem, for in memory plotting
eval { compileAndRun ("$plplot_include_path -L$plplot_lib_path -lplplotd", 0, <<'EOC'); 
#include <plplot.h>
main () {  plsmem (0, 0, NULL); }
EOC
};
if ($@) {
  print "\tplsmem function not available, no in-memory plotting available!\n";
  open (OPT, ">>OPTIONS!");
  print OPT "NOMEM!\n";
  close OPT;
} else {
  print "\tplsmem function found, in-memory plotting available!\n";
}

# Test if PLplot has plsvect, for in vector plotting
eval { compileAndRun ("$plplot_include_path -L$plplot_lib_path -lplplotd", 0, <<'EOC'); 
#include <plplot.h>
main () {  plsvect (NULL, NULL, 0, 0); }
EOC
};
if ($@) {
  print "\tplsvect function not found, no vector plotting available!\n";
  open (OPT, ">>OPTIONS!");
  print OPT "NOVECT!\n";
  close OPT;
} else {
  print "\tplsvect function found, vector plotting available!\n";
}

# Test if PLplot has support for alpha transparency, found in versions 5.9.0 and higher
eval { compileAndRun ("$plplot_include_path -L$plplot_lib_path -lplplotd",0,<<'EOC');
#include <plplot.h>
main () { c_plgcol0a(0, 0, 0, 0, NULL);}
EOC
};
if ($@){
    print "\tc_plgcol0a function not found, no alpha transparency available (need PLplot 5.9.0 or higher)!\n";
    open (OPT,">>OPTIONS!");
    print OPT "NOALPHA!\n";
    close OPT;
} else {
    print "\tc_plgcol0a function found, alpha transparency available!\n";
}

# Test if PLplot has 5 argument plpoly3 (5.1.1 and earlier) or 6 argument (5.2.0 and later)
eval { compileAndRun ("$plplot_include_path -L$plplot_lib_path -lplplotd", 0, <<'EOC'); 
#include <plplot.h>
main () {  plpoly3 (0, NULL, NULL, NULL, NULL); }
EOC
};
if ($@) {
  print "\tplpoly3 needs 6 arguments => version 5.2.0 or later!\n";
  open (OPT, ">>OPTIONS!");
  print OPT "PLPOLY3 = 6\n";
  close OPT;
} else {
  print "\tplpoly3 needs 5 arguments => version 5.1.1 or earlier!\n";
  open (OPT, ">>OPTIONS!");
  print OPT "PLPOLY3 = 5\n";
  close OPT;
}  

$PDL::Config{WITH_PLPLOT} = 1;

#
## new way to get writemakefile args...
#
@pack = (["plplot.pd",PLplot,PDL::Graphics::PLplot]);

%hash = pdlpp_stdargs_int(@::pack);  # use this for module inside PDL source
###%hash = pdlpp_stdargs(@::pack);          # use this for separate module          

$hash{'PREREQ_PM'} = { PDL => 0 };
$hash{'OPTIMIZE'}  = '-g'; # If you want to debug, uncomment this.
$hash{'VERSION'}   = '0.38';
$hash{'LIBS'}      = [$libs];
# add this to allow one to use the /ops/tools/lib plplot libs even when
# another plplot is installed in /usr/local/lib. D. Hunt 6/4/2004
$hash{'LDDLFLAGS'} = " -shared -L$plplot_lib_path -L/usr/local/lib "; 
$hash{'INC'}      .= " $plplot_include_path";
$hash{'clean'}     = {'FILES'  =>
			  'PLplot.pm PLplot.xs PLplot.o PLplot.c PLplot.bs *.xfig temp* OPTIONS! Makefile.old'};

#my $postamble = pdlpp_postamble(@::pack); # use for external module
my $postamble = pdlpp_postamble_int(@::pack); # use for install inside PDL distro

WriteMakefile(%hash);

# Add genpp rule
sub MY::postamble { $postamble };

sub compileAndRun {
  my $flags = shift;
  my $run   = shift;
  my $code  = shift;
  
  open (OUT, ">temp.c");
  print OUT $code;
  close OUT;
  unlink './temp'; # since we work out whether the compile succeeds by presence of this
  unlink './temp.exe'; # since we work out whether the compile succeeds by presence of this
  
  my $cc = $Config{cc};
  my $ccflags = $Config{ccflags};
  # print "Trying: $cc $ccflags $flags temp.c -o temp\n";
  system "$cc $ccflags $flags temp.c $flags -o temp > /dev/null 2>&1";
  unlink('./temp.c');
  die "Cannot compile test program: $code" unless (-e 'temp' or -e 'temp.exe');
  return unless ($run);

  my $res = `./temp`;
  unlink ('./temp');
  return $res;
}

sub buildDummy {
  my $msg = shift;
  $msg .= "\n      Not building PDL::Graphics::PLplot\n";
  warn $msg . "\n";
  $msg =~ s/\n//g;
  write_dummy_make( $msg );
  $PDL::Config{WITH_PLPLOT} = 0;
}
