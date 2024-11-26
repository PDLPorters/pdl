# Copyright (C) 1998 Tuomas J. Lukka.
# All rights reserved, except redistribution
# with PDL under the PDL License permitted.

package PDL::Demos::General;

sub info {('pdl', 'Introduction -- a tour of PDL')}

my @demo = (
[comment => q|
Welcome to a short tour of PDL's capabilities.

This tour shows some of the main selling points
of PDL. However, because we want this script to
run everywhere, it doesn't show off modules which
require external modules for use, including those
supporting 3D graphics. You can list all the demos
available on your system by doing "demo" at the
'pdl>' prompt.

Note that your own scripts must start with

        use PDL;

to work properly, so that you can simply say

        perl script.pl

or you can just try some of the commands illustrated
in the demos by just retyping them at the perldl or pdl
'pdl>' command prompt.
|],

[act => q|
$x = zeroes 5,5; # 5x5 matrix
print $x;
|],

[act => q|
# Now, don't think that the number of dimensions is limited
# to two:
$m = zeroes(3,2,2); # 3x2x2 cube
print $m;
|],

[act => q|
$x ++;      # Operators like increment work..
print $x;
|],

[act => q|
# xvals and yvals (yes, there is also zvals...)
# give you ndarrays which give the coordinate value.
$y = xvals $x;
print $y;
|],

[act => q|
# So you can do things like
$y = $x + 0.1 * xvals($x) + 0.01 * yvals($x);
print $y;
|],

[act => q|
# Arithmetic operations work:
$x = xvals(10) / 5;
print $x,"\n";
print ((sin $x),"\n");
|],

[act => q|
# You can also take slices:
print $y;
print $y->slice(":,2:3");  # rows 2 and 3
|],
[act => q|
print $y->slice("2:3,:");  # or columns 2 and 3
|],

[act => q|
print $y;
print $y->diagonal(0,1),"\n"; # 0 and 1 are the dimensions
|],

[act => q|
# One of the really nifty features is that the
# slices are actually references back to the original
# ndarray:
$diag = $y->diagonal(0,1);
print $y;
print $diag,"\n";
$diag+=100;
print "AFTER:\n";
print $diag,"\n";
print "Now, guess what \$y looks like?\n";
|],

[act => q|
# Yes, it has changed:
print $y;
|],

[act => q|
# Another example (we only modify elements 0,2 and 4 of
# each row):
$t = $y->slice("0:4:2"); $t += 50;
print $y;
|],

[act => q|
# There are lots of useful functions in e.g. PDL::Primitive
# and PDL::Slices - we can't show you all but here are some
# examples:

print $y;
print $y->sum, "\n";
print $y->sumover,"\n"; # Only over first dim.
|],

[act => q|
print $y->transpose;
print $y->minimum,"\n"; # over first dim.
print $y->min,"\n";
|],

[act => q|
srandom(5);
print $y->random;
|],

[act => q|
      # Here are some more advanced tricks for selecting
      # parts of 1-D vectors:
$x = (xvals 12)/3;
$i = which(sin($x) > 0.5);   # Indices of those sines > 0.5
print $x,"\n";
print $i,"\n";
print $x->index($i),"\n";
      # and we can have the effect of the last command in one
      # go using 'where' instead of 'which' and 'index' as in
print $x->where(sin($x) > 0.5),"\n";
      # and finally take the sin of these elements
      # (to show that these are indeed the correct ones)
print sin($x->index($i)),"\n";
|],

[comment => q|
We hope you enjoyed these demos illustrating some
of the basic capabilities of PDL.

We encourage you to play with these commands in
the perldl shell and use its online help support
to find out more about these and other commands and
features of PDL.

Just type 'help' to get started.
|],
);

sub demo { @demo }

1;
