#!/usr/local/bin/perl

# Example of how to use callext() - also see callext.c

use PDL;
use PDL::CallExt;

use PDL::Core ':Internal'; # For topdl()

print "1..1\n";

kill INT,$$ if $ENV{UNDER_DEBUGGER}; # Useful for debugging.

sub ok {
        my $no = shift ;
        my $result = shift ;
        print "not " unless $result ;
        print "ok $no\n" ;
}

sub approx {
        my($a,$b) = @_;
        $c = abs($a-$b);
        $d = max($c);
        $d < 0.01;
}

# Compile the code

callext_cc("t/callext.c", "-IBasic/Core", '', "t/callext.so");

my $y = sequence(5,4)+2;  # Create PDL
my $x = $y*20+100;        # Another

my $try    = loglog($x,$y);
my $correct = log(float($x))/log(float($y));

print "Try = $try\n";
print "Correct = $correct\n";
ok( 1, approx($try, $correct) );

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

   callext("t/callext.so", "loglog_ext", $ret, $y);

   return $ret;
}
