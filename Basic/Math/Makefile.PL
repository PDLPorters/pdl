
# Makefile.PL for PDL::Primitive module.

# Use this as a template for the Makefile.PL for
# any external PDL module.

use ExtUtils::MakeMaker;
use Config;

PDL::Core::Dev->import();

# Files for each routine (.c assumed)

%FileToUse = qw( acosh acosh asinh asinh atanh atanh
             	 erf ndtr erfc ndtr j0 j0 j1 j1 jn jn
	     	 y0 j0 y1 j1 yn yn ndtri ndtri
		 blas blas eigens eigens simq simq rint rint
               nan quiet_nan infinity infinity
	       );
%included = ();

# test for library features 
my (@sfuncs) = qw(nan infinity);
my (@ufuncs2) = qw(acosh asinh atanh erf erfc rint);
my (@besufuncs) = qw(j0 j1 y0 y1);
my (@besbifuncs) = qw(jn yn);
my ($libs) = '-lm';
$libs = '-L/opt/SUNWspro/lib -lsunmath '.$libs 
      if (($^O eq 'solaris' or $^O eq 'sunos') and -d "/opt/SUNWspro/lib/");

@need = qw(ndtri blas eigens simq); # List of routine we need because system
            # does not have them

# Test for absence of unary functions

$dir = $ENV{PWD}.'/Basic/Math';

my $testfile = "pdl$$";
my $HIDE = '>/dev/null 2>&1';
foreach (@sfuncs) {
    open FILE,">/tmp/$testfile.c";
    print FILE <<"EOF";
#include <stdio.h>
#include "mconf.h"
main() {printf("%lg",$_());}
EOF
    close FILE;
    push @need, $_ if 
      system("$Config{cc} -I$dir -o /tmp/$testfile /tmp/$testfile.c $libs $HIDE")
      !=0;
}

foreach (@ufuncs2) {
    open FILE,">/tmp/$testfile.c";
    print FILE <<"EOF";
#include <stdio.h>
#include "mconf.h"
main() {printf("%lg",$_(1.));}
EOF
    close FILE;
    push @need, $_ if 
      system("$Config{cc} -I$dir -o /tmp/$testfile /tmp/$testfile.c $libs $HIDE")
      !=0;
}

# Test for absence of besfuncs

foreach (@besufuncs) {
    open FILE,">/tmp/$testfile.c";
    print FILE <<"EOF";
#include <stdio.h>
#include "mconf.h"
main() {printf("%lg",$_(1.));}
EOF
    close FILE;
    push @need, $_  if 
      system("$Config{cc} -I$dir -o /tmp/$testfile /tmp/$testfile.c $libs $HIDE")
      !=0;
}
foreach (@besbifuncs) {
    open FILE,">/tmp/$testfile.c";
    print FILE <<"EOF";
#include <stdio.h>
#include "mconf.h"
main() {printf("%lg",$_(1.,1.));}
EOF
    close FILE;
    push @need, $_ if 
      system("$Config{cc} -I$dir -o /tmp/$testfile /tmp/$testfile.c $libs $HIDE")
      !=0;
}
unlink "/tmp/$testfile","/tmp/$testfile.c";

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
