use strict;
use warnings;
use ExtUtils::MakeMaker;
use File::Basename;
use Config;

use File::Spec;
sub cdir { return File::Spec->catdir(@_)}

sub is_sys_func {
  my ( $code, $libs, $dir ) = @_;
  trylink( '', qq{#include "$dir/mconf.h"}, $code, $libs );
}

# Files for each routine (.c assumed)

my %source = qw(
	      j0 j0
	      j1 j1
	      jn jn
	      y0 j0
	      y1 j1
	      yn yn
	      erfi ndtri
	      ndtri ndtri
	      nan quiet_nan
	      infinity infinity
	      polyroots cpoly
	    );
my @keys = sort keys %source;
my %included = ();

# test for library features 
my (@sfuncs) = qw(nan infinity);
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
      print "Couldn't find sunmath library in standard places\n";
      print "If you can find libsunmath.a or libsunmath.so\n";
      print "please let us know at pdl-devel\@lists.sourceforge.net\n";
   }
}

# Test for absence of unary functions

use Cwd;
my $mmdir = my $mdir = cdir 'Basic','Math';
$mmdir =~ s/\\/\\\\/g;
my $dir = File::Spec->canonpath(cwd);
$dir = cdir $dir, $mdir unless $dir =~ /$mmdir$/;

foreach (@sfuncs) {
    $source{$_} = 'system' if is_sys_func( "$_();", $libs, $dir );
}

# Test for absence of besfuncs

foreach (@besufuncs) {
    if ( is_sys_func( "$_(1.);", $libs, $dir ) ) {
        $source{$_} = 'system';
        next if $_ ne 'y0';
    }
}

foreach (@besbifuncs) {
    next if ! exists $source{$_};     # May have been deleted in buggy case
    $source{$_} = 'system' if is_sys_func( "$_(1,1.);", $libs, $dir );
}

my @pack = (["math.pd", qw(Math PDL::Math)]);
my %hash = pdlpp_stdargs_int(@pack);

my %seen = (); # Build object file list
foreach my $func (@keys) {
   my $file = $source{$func};
   next if $file eq 'system';
   die "File for function $func not found\n" if $file eq '';
   $hash{OBJECT} .= " $file\$(OBJ_EXT)" unless $seen{$file}++;
   $hash{DEFINE} .= ' -DMY_'.uc($func);
}

# Add support routines
$hash{OBJECT} .= " const\$(OBJ_EXT) mtherr\$(OBJ_EXT) polevl\$(OBJ_EXT)";

$hash{LIBS}->[0] .= " $libs";

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};

WriteMakefile(%hash);
