
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

%FileToUse = qw( acosh acosh asinh asinh atanh atanh
             	 erf ndtr erfc ndtr j0 j0 j1 j1 jn jn
	     	 y0 j0 y1 j1 yn yn ndtri ndtri
		 blas blas eigens eigens simq simq rint rint
               nan quiet_nan infinity infinity svd svd
	       );
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
       if (-e "$d/libsunmath.so") {
           $libs = "-L$d -lsunmath $libs";
           last;
       }
    }
}
@need = qw(ndtri blas eigens simq svd y0 yn); # List of routine we need because system
            # does not have them

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
    push @need, $_ if 
      system("$Config{cc} -I$dir -o $te $tc $libs $HIDE")
      !=0;
}

foreach (@ufuncs2) {
    open FILE,">$tc";
    print FILE <<"EOF";
#include <stdio.h>
#include "mconf.h"
main() {printf("%lg",$_(1.)); exit(0); }
EOF
    close FILE;
    push @need, $_ if 
      system("$Config{cc} -I$dir -o $te $tc $libs $HIDE")
      !=0;
}

# Test for absence of besfuncs

foreach (@besufuncs) {
    open FILE,">$tc";
    print FILE <<"EOF";
#include <stdio.h>
#include "mconf.h"
main() {printf("%lg",$_(1.)); exit(0); }
EOF
    close FILE;
    push @need, $_  if 
      system("$Config{cc} -I$dir -o $te $tc $libs $HIDE")
      !=0;
}
foreach (@besbifuncs) {
    open FILE,">$tc";
    print FILE <<"EOF";
#include <stdio.h>
#include "mconf.h"
main() {printf("%lg",$_(1.,1.)); exit(0); }
EOF
    close FILE;
    push @need, $_ if 
      system("$Config{cc} -I$dir -o $te $tc $libs $HIDE")
      !=0;
}
unlink "$te","$tc";

print "\nUsing distribution version of functions: @need\n";

print "Using system version of functions: ";
for $func (@sfuncs,@ufuncs2,@besufuncs,@besbifuncs) {
  print $func," " unless grep {$func eq $_} @need;
}
print "\n\n";

@pack = (["math.pd",Math,PDL::Math]);
%hash = pdlpp_stdargs_int(@::pack);

%seen = (); # Build object file list
for $func (@need) {
   die "File for function $func not found\n" if $FileToUse{$func} eq '';
   $file = $FileToUse{$func};
   $hash{OBJECT} .= " $file\$(OBJ_EXT)" unless $seen{$file}++;
   $hash{DEFINE} .= ' -DMY_'.uc($func);
}

# Add support routines if we are using the distn versions

$hash{OBJECT} .= " const\$(OBJ_EXT) mtherr\$(OBJ_EXT) polevl\$(OBJ_EXT)"
   if scalar(@need)>0;

$hash{LIBS} = [$libs];
WriteMakefile(%hash);

sub MY::postamble {
	pdlpp_postamble_int(@::pack);
}  # Add genpp rule


