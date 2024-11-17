use strict;
use warnings;
use ExtUtils::MakeMaker;
use File::Basename;
use Config;

sub is_sys_func {
  my ( $code, $libs ) = @_;
  trylink( '', qq{#include "mconf.h"}, $code, $libs );
}

# Files for each routine (.c assumed)
my %source = qw(
  j0 j0
  j1 j1
  jn jn
  y0 j0
  y1 j1
  yn yn
);
my @keys = sort keys %source;

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

# Test for presence of besfuncs
$source{$_} = 'system' for grep is_sys_func("$_(1.);", $libs), qw(j0 j1 y0 y1);
$source{$_} = 'system' for grep is_sys_func("$_(1,1.);", $libs), qw(jn yn);

my @pack = (["math.pd", qw(Math PDL::Math)]);
my %hash = pdlpp_stdargs_int(@pack);

my %seen; # Build object file list
foreach my $func (@keys) {
   my $file = $source{$func};
   next if $file eq 'system';
   die "File for function $func not found\n" if $file eq '';
   $hash{OBJECT} .= " $file\$(OBJ_EXT)" unless $seen{$file}++;
}

# Add support routines
$hash{OBJECT} .= join '', map " $_\$(OBJ_EXT)", qw(const mtherr polevl cpoly ndtri);

$hash{LIBS}[0] .= " $libs";

undef &MY::postamble; # suppress warning
*MY::postamble = sub {
	pdlpp_postamble_int(@pack);
};

WriteMakefile(%hash);
