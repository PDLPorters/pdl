
# Makefile.PL for PDL::Primitive module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

use ExtUtils::MakeMaker;
use File::Basename;
use Config;

require File::Spec;
$fs = 'File::Spec';
sub cdir { return $fs->catdir(@_)}
sub cfile { return $fs->catfile(@_)}

sub is_sys_func {
  my ( $code, $libs ) = @_;
  trylink( '', qq{#include "$dir/mconf.h"}, $code, $libs );
}

PDL::Core::Dev->import();

# Files for each routine (.c assumed)

%source = qw( blas blas
	      eigens eigens
	      simq simq
	      svd svd
	    );
@keys = sort keys %source;
%included = ();

# test for library features 
my (@sfuncs) = qw(nan infinity);
my (@ufuncs2) = qw(acosh asinh atanh erf erfc rint);
my (@besufuncs) = qw(j0 j1 y0 y1);
my (@besbifuncs) = qw(jn yn);
my ($libs) = $^O =~ /MSWin/ ? '' : $^O =~ /cygwin/ ? getcyglib('m') : '-lm';
if ($^O eq 'solaris' or $^O eq 'sunos') {
   # try to guess where sunmath is
   my @d = split /:+/, $ENV{LD_LIBRARY_PATH};
   my $ok = 0;
   for my $d (@d) {
      if (-e "$d/libsunmath.so" or -e "$d/libsunmath.a" ) {
          $libs = "-lsunmath $libs";
	  $ok = 1;
          last;
      }
   }
   if (!$ok) {
      print "libsunmath not found in LD_LIBRARY_PATH: looking elsewhere\n";

      # get root directory of compiler; may be off of there
      my @dirs = ();
      foreach my $p ( split(':', $ENV{'PATH'} ) )
      {
	next unless -e "$p/$Config{cc}";
	push @dirs, dirname($p) . '/lib';
	last;
      }

      push @dirs, '/opt/SUNWspro/lib'; # default location if all else fails
      for my $d ( @dirs ) {
         if (-e "$d/libsunmath.so") {
            $libs = "-R$d -L$d -lsunmath $libs";
             $ok = 1;
             last;
         } 
         if (-e "$d/libsunmath.a") {
            $libs = "-L$d -lsunmath $libs";
            $ok = 1;
            last;
         } 
      }
   }

   if (!$ok) {
      print "Couldn't find sunmath library in standard places\n\n";
      print "If you can find libsunmath.a or libsunmath.so\n";
      print "please let us know at pdl-porters\@jach.hawaii.edu\n\n";
   }
}

# Test for absence of unary functions

use Cwd;
$mmdir = $mdir = cdir 'Basic','Math';
$mmdir =~ s/\\/\\\\/g;
$dir = $fs->canonpath(cwd);
$dir = cdir $dir, $mdir unless $dir =~ /$mmdir$/;
$td = $^O =~ /MSWin/ ? 'TEMP' : 'tmp';
$tempd = defined $ENV{TEMP} ? $ENV{TEMP} :
            defined $ENV{TMP} ? $ENV{TMP} :
                           cdir($fs->rootdir,$td);

foreach (@sfuncs) {
    $source{$_} = 'system' if is_sys_func( "$_();", $libs );
}

foreach (@ufuncs2) {
    $source{$_} = 'system' if is_sys_func( "$_(1.);", $libs );
}

# Test for absence of besfuncs

foreach (@besufuncs) {
    if ( is_sys_func( "$_(1.);", $libs ) ) {
        $source{$_} = 'system';
        next if $_ ne 'y0';
# Need to test for buggy glibc
        open (RES,"$te |");
        my ($n) = <RES>;
        close RES;
#        print "Done y0 test, received $n\n";
        $n /= 0.088257;               # This _should_ be the answer
        $n -= 1.;
        if ($n*$n > 1e-3) {
            delete $source{$_};
            delete $source{'yn'};
            $source{'fixy0'} = 'j0';
            $source{'fixyn'} = 'yn';
            @keys = sort keys %source;
        }
    }
}

foreach (@besbifuncs) {
    next if ! exists $source{$_};     # May have been deleted in buggy case
    $source{$_} = 'system' if is_sys_func( "$_(1,1.);", $libs );
}

print "Source of functions\nSystem:      ";
foreach (@keys) {
    print " $_" if $source{$_} eq 'system';
}
print "\nDistribution:";
foreach (@keys) {
    print " $_" if $source{$_} ne 'system';
}
print "\n\n";

@pack = (["matrixops.pd",'MatrixOps','PDL::MatrixOps']);
%hash = pdlpp_stdargs_int(@::pack);

%seen = (); # Build object file list
foreach $func (@keys) {
   $file = $source{$func};
   next if $file eq 'system';
   die "File for function $func not found\n" if $file eq '';
   $hash{OBJECT} .= " $file\$(OBJ_EXT)" unless $seen{$file}++;
   $hash{DEFINE} .= ' -DMY_'.uc($func);
}

# Add support routines
$hash{OBJECT} .= " mtherr\$(OBJ_EXT)";

$hash{LIBS}->[0] .= " $libs";
WriteMakefile(%hash);

sub MY::postamble {
	pdlpp_postamble_int(@::pack);
}  # Add genpp rule


