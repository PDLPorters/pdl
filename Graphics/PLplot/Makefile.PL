# Makefile.PL for a package defined by PP code.

use ExtUtils::MakeMaker;
use Config;
use File::Spec;
use Data::Dumper;

# comment this block out for PDL internal version
#eval {
#  require PDL::Core::Dev;  # needed for stand-alone version of package.
#                           # Need to comment out
#                           # for package included in PDL distro.
#
#  PDL::Core::Dev->import;
#};
#my $pdl_installed = ($@) ? 0 : 1; # make always == 1 for install from PDL distro.
#print "PDL ", ($pdl_installed ? 'is' : 'is not'), " installed\n";
# end comment this block out for PDL internal version

my $pdl_installed = 1; # uncomment for PDL-internal version

if (!$pdl_installed) {
  # write dummy makefile if PDL not installed
  WriteMakefile(
		'NAME'         => 'PDL::Graphics::PLplot',
		'PREREQ_PM'    => { PDL => 0 },
		);
  exit;
}

#
## Search for plplot library and include file
#
print "\nChecking PDL::Graphics::PLplot...\n";

unlink ("OPTIONS!"); # remove file used to communicate with plplot.pd

# on OS-X the library is called libplplotd.dylib, so we
# use $Config{"so"} to get the extension name
#
my $libname = "libplplotd." . $Config{"so"};
my $libname_static = "libplplotd.a";
my $incname = "plplot.h";
my $devnull = File::Spec->devnull(); # Portable null device.

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
		    $ENV{PLPLOT_LIBDIR},
		    $PDL::Config{WHERE_PLPLOT_LIBS},
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
		    $ENV{PLPLOT_INCDIR},
		    $PDL::Config{WHERE_PLPLOT_INCLUDE},
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
  buildDummy ("Cannot find good set of libraries for linking PLplot, try fiddling perldl.conf\n");
  return if (caller); exit;
}

# Try linking with library.  New if blocks should be added if necessary
my $libs;
foreach my $l (
	       "-L$plplot_lib_path -lplplotd -lX11",
	       "-L$plplot_lib_path -lplplotd",
             "-L$plplot_lib_path -lplplotd -lqsastime -lcsirocsa -lgdi32 -lcomdlg32",
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

#
## Tests for various PLplot features, present in different versions of PLplot
#
my %plversion = (); # Information on which PLplot functions/features available.
$plversion{'plsmem'}      = test_func_exists('plsmem', 'in-memory plotting', 'plsmem( 0, 0, NULL);');
$plversion{'plsvect'}     = test_func_exists('plsvect', 'vector plotting',   'plsvect (NULL, NULL, 0, 0);');
$plversion{'c_plgcoloa'}  = test_func_exists('c_plgcoloa', 'alpha transparency', 'c_plgcol0a(0, 0, 0, 0, NULL);');
$plversion{'c_plseed'}    = test_func_exists('c_plseed',   'random numbers',     'c_plseed (0 );');
$plversion{'c_plsfont'}   = test_func_exists('c_plsfont',  'set font',           'c_plsfont (0, 0, 0);');
$plversion{'c_plimagefr'} = test_func_exists('c_plimagefr','2d matrix',
                                             'c_plimagefr (NULL, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NULL, NULL);' );
$plversion{'c_pllegend'}  = test_func_exists('c_pllegend', 'Plot legends',
                                             'c_pllegend(NULL,NULL,0,   0,   0.0, 0.0, 0.0, 0,   0,   0,
                                                         0,   0,   0,   NULL,0.0, 0.0, 0.0, 0.0, NULL,NULL,
                                                         NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);');
# Write these options to a file--used by plplot.pd during the 'make' step
open my $fh, '>', 'OPTIONS!' or die "Cannot write to OPTIONS! file";
print  {$fh} Dumper(\%plversion);
close   $fh;

$PDL::Config{WITH_PLPLOT} = 1;


#
## new way to get writemakefile args...
#
@pack = (["plplot.pd",PLplot,PDL::Graphics::PLplot]);

#%hash = pdlpp_stdargs_int(@::pack);  # use this for module inside PDL source
%hash = pdlpp_stdargs(@::pack);          # use this for separate module          

# $hash{'PREREQ_PM'} = { PDL => 0 };
$hash{'OPTIMIZE'}  = '-g'; # If you want to debug, uncomment this.
$hash{'VERSION_FROM'}   = 'plplot.pd';
$hash{'META_MERGE'}   = {provides => {PDL::Graphics::PLplot => {file => 'plplot.pd', version => 0.57}}};

$hash{'LIBS'}      = [$libs];
# add this to allow one to use the /ops/tools/lib plplot libs even when
# another plplot is installed in /usr/local/lib. D. Hunt 6/4/2004
$hash{'LDDLFLAGS'} = $^O eq 'darwin' ? " -bundle -undefined dynamic_lookup -L$plplot_lib_path " : " -shared -L$plplot_lib_path -L/usr/local/lib ";

$hash{'INC'}      .= " $plplot_include_path";
$hash{'clean'}     = {'FILES'  =>
                        'PLplot.pm PLplot.xs PLplot.o PLplot.c PLplot.bs *svg.* *.xfig temp* OPTIONS! Makefile.old'};

#my $postamble = pdlpp_postamble(@::pack); # use for external module
my $postamble = pdlpp_postamble_int(@::pack); # use for install inside PDL distro

WriteMakefile(%hash);

# Add genpp rule
sub MY::postamble { $postamble };

sub test_func_exists {

    my ( $func, $comment, $eval, $option ) = @_;

    $option = $func unless defined $option;

    # Test if PLplot has plsmem, for in memory plotting
    eval { compileAndRun ("$plplot_include_path $libs", 0, <<"EOC");
#include <plplot.h>
main () {  $eval }
EOC
       };

    my $have_func;
    if ($@)
    {
	print "\t$func function not found, no $comment available\n";
	$have_func = 0;
    }
    else
    {
	print "\t$func function found, $comment available!\n";
	$have_func = 1;
    }

    return $have_func;
}

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

  # The duplication of $flags seems to be necessary, as some systems
  # need $flags before temp.c and some after.  D. Hunt 2/9/2011
  system "$cc $ccflags $flags temp.c $flags -o temp > $devnull 2>&1";
  unlink('./temp.c');
  die "Cannot compile test program: $code" unless (-e 'temp' or -e 'temp.exe');
  unless ($run){
      unlink './temp';
      unlink './temp.exe';
      return;
  }
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
