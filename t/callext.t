#!/usr/local/bin/perl

END { unlink 't/callext.pdb';}; # In case we build a 2nd time,
                                # but using a different Microsoft compiler

# Example of how to use callext() - also see callext.c

use strict;
use Test;
use Config;

BEGIN { plan tests => 1;
}
use PDL;
use PDL::CallExt;

use PDL::Core ':Internal'; # For topdl()
use Config;
use File::Spec;

kill 'INT',$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub tapprox {
        my($a,$b) = @_;
        my $c = abs($a-$b);
        my $d = max($c);
        $d < 0.01;
}

# Create the filenames
my $cfile = File::Spec->catfile('t', 'callext.c');
# include the pdlsimple.h that's in blib.
my $inc = File::Spec->catdir('blib', 'lib', 'PDL', 'Core');
my $out   = File::Spec->catfile('t', 'callext.'.$Config{dlext});

# Compile the code

my @cleanup = ();
END { unlink @cleanup; }
push @cleanup, File::Spec->catfile('t', 'callext'.$Config{obj_ext}), $out;
callext_cc($cfile, qq{"-I$inc"}, '', $out);

my $y = sequence(5,4)+2;  # Create PDL
my $x = $y*20+100;        # Another

my $try    = loglog($x,$y);
my $correct = log(float($x))/log(float($y));

print "Try = $try\n";
print "Correct = $correct\n";
ok( tapprox($try, $correct) );

# Return log $x to base $y using callext() routine -
# perl wrapper makes this nice and easy to use.

sub loglog {

   die 'Usage: loglog($x,$y)' if scalar(@_)!=2;

   # Tips:
   #
   # (i)  topdl() forces arguments to be pdl vars even
   #      if ordinary numbers are passed
   #
   # (ii) float() forces the pdl vars to be float precision
   #      thus matching the C routine.

   my $x = float(topdl(shift));
   my $y = float(topdl(shift));

   my $ret = $x->copy; # Make copy of $x to return

   print "X = $x\n";
   print "Y = $y\n";

   my $ldfile =
   callext($out, "loglog_ext", $ret, $y);

   return $ret;
}
