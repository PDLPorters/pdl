#!/usr/local/bin/perl

# Example of how to use callext() - also see testcallext.c

use PDL;

print "\nDon't forget to recompile the C code!!!\n\n";

$y = sequence(10)+2;   # Create PDL
$x = $y*20+100;        # Another

print $x, "\n";
print $y, "\n";
print loglog($x,$y), "\n";

# Return log $x to base $y using callext() routine - 
# perl wrapper makes this nice and easy to use.

sub loglog {

   die 'Usage: loglog($x,$y)' if scalar(@_)!=2;

   # Tips: 
   #
   # (i)  topdl() forces arguments to be pdl vars even
   #      if ordinary numbers are passed
   #
   # (ii) double() forces the pdl vars to be double precision
   #      thus matching the C routine.
 
   my $x = double(topdl(shift));
   my $y = double(topdl(shift));

   my $ret = pdl($x); # Make copy of $x to return

   callext("./testcallext.so", "loglog_ext", $ret, $y);

   return $ret;
}
