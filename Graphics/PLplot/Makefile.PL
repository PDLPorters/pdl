# Makefile.PL for a package defined by PP code.

use PDL::Core::Dev;            # Pick up development utilities
use ExtUtils::MakeMaker;
use Config;


# 
## Search for plplot library and include file
#

my $found_plplot = (defined($PDL_CONFIG{WITH_PLPLOT}) && $PDL_CONFIG{WITH_PLPLOT} == 0) ? 0 : 1;
print "User requested no PLplot interface!\n" if ($found_plplot == 0);

foreach my $libdir (
                    '/usr/local/plplot/lib',
		    '/usr/local/lib',
		    '/opt/local/lib',
		    '/usr/lib',
		    '/opt/lib',
		    '/ops/tools/lib',
		    $PDL_CONFIG{WHERE_PLPLOT_LIB},
		                  # Add new library paths here!!
		    ) {

  if (-e "$libdir/libplplotd.so") {
    $plplot_lib_path = $libdir;
    $ENV{LD_LIBRARY_PATH} .= $libdir;
    last;
  }


}

unless (defined ($plplot_lib_path)) {
  print "          Cannot find plplot library (libplplotd.so), skipping PDL::Graphics::PLplot\n";
  $found_plplot = 0;
}

foreach my $incdir (
                    '/usr/local/plplot/include',
                    '/usr/local/plplot/include/plplot',
		    '/usr/local/include/plplot',
		    '/usr/local/include',
		    '/opt/local/include',
		    '/usr/include',
		    '/opt/include',
		    '/ops/tools/include/plplot',
		    '/ops/tools/include',
		    $PDL_CONFIG{WHERE_PLPLOT_INCLUDE},
		                  # Add new header paths here!!
		    ) {

  if (-e "$incdir/plplot.h") {
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
  print "          Cannot find plplot header file, plplot.h, skipping PDL::Graphics::PLplot\n";
  $found_plplot = 0;
}

# Test that PLplot was compiled --with-double (IE, a PLFLT is a double)
my $size = compileAndRun ("$plplot_include_path", <<'EOC');
#include <plplot.h>
main () {  printf  ("%d", sizeof (PLFLT)); }
EOC

unless ($size == 8) {
  print "          Sizeof(PLFLT) must be 8. PLplot must be compiled --with-double (IE ./configure --with-double)\n";
  $found_plplot = 0;
} 

# Test if PLplot has plsmem, for in memory plotting
eval { compileAndRun ("$plplot_include_path -L$plplot_lib_path -lplplotd", <<'EOC'); 
#include <plplot.h>
main () {  plsmem (0, 0, NULL); }
EOC
};
unlink ("NOMEM!");
if ($@) {
  print "plsmem function not available, no in-memory plotting available!\n";
  system "touch NOMEM!";
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
    print "Succeeded linking PLplot with $libs\n";
    last;
  } 
}

unless (defined($libs)) {

  print "Cannot find good set of libraries for linking PLplot, try fiddling perldl.conf\n";
  $found_plplot = 0;
}

$VERSION = '0.19';

unless ($found_plplot) {
  my $msg = "\n      Not building PDL::Graphics::PLplot\n";
  warn $msg . "\n";
  $msg =~ s/\n//g;
  write_dummy_make( $msg );
  $PDL_CONFIG{WITH_PLPLOT} = 0;
} else {

  $PDL_CONFIG{WITH_PLPLOT} = 1;
  $package = ["plplot.pd",PLplot,PDL::Graphics::PLplot];
  WriteMakefile(
		'NAME'  	     => 'PDL::Graphics::PLplot',
		'CCFLAGS'      => "-g",
		'VERSION_FROM' => 'Makefile.PL',
		'TYPEMAPS'     => [&PDL_TYPEMAP()],
		'OBJECT'       => 'PLplot.o ',
		'PM'           => { 'PLplot.pm'   => '$(INST_LIBDIR)/PLplot.pm' },
		'INC'          => &PDL_INCLUDE()." $plplot_include_path", 
		'LIBS'         => [$libs],  
		'clean'        => {'FILES'  => 
				     'PLplot.pm PLplot.xs PLplot.o PLplot.c PLplot.bs *.xfig temp* Makefile.old'},
		);
  
  sub MY::postamble { pdlpp_postamble($package); }	
}

sub compileAndRun {
  my $flags = shift;
  my $code  = shift;
  
  open (OUT, ">temp.c");
  print OUT $code;
  close OUT;
  
  my $cc = $Config{cc};
  my $ccflags = $Config{ccflags};
  # print "Trying: $cc $ccflags $flags temp.c -o temp\n";
  system "$cc $ccflags $flags temp.c -o temp > /dev/null 2>&1";
  die "Cannot compile test program: $code" unless (-e 'temp');
  my $res = `./temp`;
  unlink ('./temp', './temp.c');
  return $res;
}





