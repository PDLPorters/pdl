# Makefile.PL for a package defined by PP code.

use ExtUtils::MakeMaker;
use Config;

# use PDL::Core::Dev;  # needed for stand-alone version of package.
                       # Need to comment out
                       # for package included in PDL distro.
PDL::Core::Dev->import();

use vars qw /$plplotvers/; # version of plplot used

# Pick up development utilities
PDL::Core::Dev->import();

# 
## Search for plplot library and include file
#
print "\nChecking PDL::Graphics::PLplot...\n";

unlink ("OPTIONS!"); # remove file uses to communicate with plplot.pd

# on OS-X the library is called libplplotd.dylib, so we
# use $Config{"so"} to get the extension name
#
my $libname = "libplplotd." . $Config{"so"};
my $incname = "plplot.h";

my $found_plplot =
    (defined($PDL::Config{WITH_PLPLOT}) && (!$PDL::Config{WITH_PLPLOT}))
    ? 0 : 1;
print "found_plplot is $found_plplot\n";

unless ($found_plplot) {
  buildDummy ("User requested no PLplot interface!");
  return if (caller); exit;
}
foreach my $libdir (
                    '/usr/local/plplot/lib',
		    '/usr/local/lib',
		    '/opt/local/lib',
		    '/usr/lib',
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
%hash = pdlpp_stdargs_int(@::pack);

$hash{'OPTIMIZE'}  = '-g'; # If you want to debug, uncomment this.
$hash{'VERSION'}   = '0.21';
$hash{'LIBS'}      = [$libs];
$hash{'INC'}      .= " $plplot_include_path";
$hash{'clean'}     = {'FILES'  =>
			  'PLplot.pm PLplot.xs PLplot.o PLplot.c PLplot.bs *.xfig temp* OPTIONS! Makefile.old'};

WriteMakefile(%hash);

# Add genpp rule
sub MY::postamble {
    pdlpp_postamble_int(@::pack);
}

### $package = ["plplot.pd",PLplot,PDL::Graphics::PLplot];
### WriteMakefile(
### 	      'NAME'  	     => 'PDL::Graphics::PLplot',
### 	      'CCFLAGS'      => "-g",
### 	      'VERSION_FROM' => 'Makefile.PL',
### 	      'TYPEMAPS'     => [&PDL_TYPEMAP()],
### 	      'OBJECT'       => 'PLplot.o ',
### 	      'PM'           => { 'PLplot.pm'   => '$(INST_LIBDIR)/PLplot.pm' },
### 	      'INC'          => &PDL_INCLUDE()." $plplot_include_path", 
### 	      'LIBS'         => [$libs],  
### 	      'clean'        => {'FILES'  => 
### 				   'PLplot.pm PLplot.xs PLplot.o PLplot.c PLplot.bs *.xfig temp* OPTIONS! Makefile.old'},
### 	      );
### 
### sub MY::postamble { pdlpp_postamble($package); }	

sub compileAndRun {
  my $flags = shift;
  my $run   = shift;
  my $code  = shift;
  
  open (OUT, ">temp.c");
  print OUT $code;
  close OUT;
  unlink './temp'; # since we work out whether the compile succeeds by presence of this
  
  my $cc = $Config{cc};
  my $ccflags = $Config{ccflags};
  # print "Trying: $cc $ccflags $flags temp.c -o temp\n";
  system "$cc $ccflags $flags temp.c -o temp > /dev/null 2>&1";
  die "Cannot compile test program: $code" unless (-e 'temp');
  return unless ($run);

  my $res = `./temp`;
  unlink ('./temp', './temp.c');
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
