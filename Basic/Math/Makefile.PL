
# Makefile.PL for PDL::Primitive module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

use ExtUtils::MakeMaker;
use Config;

require File::Spec;
$fs = 'File::Spec';
sub cdir { return $fs->catdir(@_)}
sub cfile { return $fs->catfile(@_)}

PDL::Core::Dev->import();

# Files for each routine (.c assumed)

%source = qw( acosh acosh asinh asinh atanh atanh
             	 erf ndtr erfc ndtr j0 j0 j1 j1 jn jn
                 y0 j0 y1 j1 yn yn erfi ndtri ndtri ndtri
		 blas blas eigens eigens simq simq rint rint
               nan quiet_nan infinity infinity svd svd
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
    # try to guess where is sunmath
    my @d = split /:+/, $ENV{LD_LIBRARY_PATH};
    for my $d (@d) {
       if (-e "$d/libsunmath.so" or -e "$d/libsunmath.a" ) {
           $libs = "-L$d -lsunmath $libs";
           last;
       }
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

my $testfile = "pdl$$";
my ($tc,$te) = map {cfile($tempd,"testfile$_")} ('.c','');
my $HIDE = $^O =~ /MSWin/ ? '' : '>/dev/null 2>&1';

foreach (@sfuncs) {
    open FILE,">$tc";
    print FILE <<"EOF";
#include <stdio.h>
#include "mconf.h"
main() {printf("%lg",$_()); exit(0); }
EOF
    close FILE;
    $source{$_} = 'system' if
      system("$Config{cc} -I$dir -o $te $tc $libs $HIDE") == 0;
}

foreach (@ufuncs2) {
    open FILE,">$tc";
    print FILE <<"EOF";
#include <stdio.h>
#include "mconf.h"
main() {printf("%lg",$_(1.)); exit(0); }
EOF
    close FILE;
    $source{$_} = 'system' if
      system("$Config{cc} -I$dir -o $te $tc $libs $HIDE") == 0;
}

# Test for absence of besfuncs

foreach (@besufuncs) {
    open FILE,">$tc";
    print FILE <<"EOF";
#include <stdio.h>
#include "mconf.h"
main() {printf("%lg\\n",$_(1.)); exit(0); }
EOF
    close FILE;
    if (system("$Config{cc} -I$dir -o $te $tc $libs $HIDE") == 0) {
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
    open FILE,">$tc";
    print FILE <<"EOF";
#include <stdio.h>
#include "mconf.h"
main() {printf("%lg",$_(1,1.)); exit(0); }
EOF
    close FILE;
    $source{$_} = 'system' if
      system("$Config{cc} -I$dir -o $te $tc $libs $HIDE") == 0;
}
unlink "$te","$tc";

print "Source of functions\nSystem:      ";
foreach (@keys) {
    print " $_" if $source{$_} eq 'system';
}
print "\nDistribution:";
foreach (@keys) {
    print " $_" if $source{$_} ne 'system';
}
print "\n\n";

@pack = (["math.pd",Math,PDL::Math]);
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
$hash{OBJECT} .= " const\$(OBJ_EXT) mtherr\$(OBJ_EXT) polevl\$(OBJ_EXT)";

$hash{LIBS} = [$libs];
WriteMakefile(%hash);

sub MY::postamble {
	pdlpp_postamble_int(@::pack);
}  # Add genpp rule


