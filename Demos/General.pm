# Copyright (C) 1998 Tuomas J. Lukka.
# All rights reserved, except redistribution
# with PDL under the PDL License permitted.

package PDL::Demos::General;
use PDL;

PDL::Demos::Routines->import();
sub comment($);
sub act($);
sub output;

sub run {

comment q|
	Welcome to a short tour of PDL's capabilities.

	This tour shows some of the main selling points
	of PDL. However, because we want this script to
	run everywhere, some modules which require external
	modules for use are explicitly excluded, namely
	 - PDL::Graphics::TriD (3D Graphics) [*]
	 - PDL::Graphics::PGPLOT (PGPLOT graphics)
	 - PDL::IO::FlexRaw (flexible raw input/output)
 	[*]: this module has its separate demos in a subdirectory.

	Note that your own scripts must start with

		use PDL;

	to work properly, so that you can simply say

                perl script.pl

        or you can just try some of the commands illustrated
        in the demos by just retyping them at the perldl or pdl
        'pdl>' command prompt.
|;

act q|
	$x = zeroes 5,5; # 5x5 matrix
	output $x;
|;

act q|
	# Now, don't think that the number of dimensions is limited
	# to two:
	$m = zeroes(3,2,2); # 3x2x2 cube
	output $m;
|;

act q|
	$x ++;      # Operators like increment work..
	output $x;
|;

act q|
	# xvals and yvals (yes, there is also zvals...)
	# give you piddles which give the coordinate value.
	$y = xvals $x;
	output $y;
|;

act q|
	# So you can do things like
	$y = $x + 0.1 * xvals($x) + 0.01 * yvals($x);
	output $y;
|;

act q|
	# Arithmetic operations work:
	$x = xvals(10) / 5;
        output $x,"\n";
	output ((sin $x),"\n");
|;

act q|
	# You can also take slices:
	output $y;
	output $y->slice(":,2:3");  # rows 2 and 3
|;
act q|
	output $y->slice("2:3,:");  # or columns 2 and 3
|;

act q|
	output $y;
	output $y->diagonal(0,1),"\n"; # 0 and 1 are the dimensions
|;

act q|
	# One of the really nifty features is that the
	# slices are actually references back to the original
	# piddle:
	$diag = $y->diagonal(0,1);
	output $y;
	output $diag,"\n";
	$diag+=100;
	output "AFTER:\n";
	output $diag,"\n";
	output "Now, guess what \$y looks like?\n";
|;

act q|
	# Yes, it has changed:
	output $y;
|;

act q|
	# Another example (we only modify elements 0,2 and 4 of
        # each row):
	$t = $y->slice("0:4:2"); $t += 50;
	output $y;
|;

act q|
	# There are lots of useful functions in e.g. PDL::Primitive
	# and PDL::Slices - we can't show you all but here are some
	# examples:

	output $y;
	output $y->sum, "\n";
	output $y->sumover,"\n"; # Only over first dim.
|;

act q|
	output $y->xchg(0,1);
	output $y->minimum,"\n"; # over first dim.
	output $y->min,"\n";
|;

act q|
	output $y->random;
|;

act q|
	# Here are some more advanced tricks for selecting
	# parts of 1-D vectors:
	$x = (xvals 12)/3;
	$i = which(sin($x) > 0.5);   # Indices of those sines > 0.5
	output $x,"\n";
	output $i,"\n";
	output $x->index($i),"\n";
             # and we can have the effect of the last command in one
             # go using 'where' instead of 'which' and 'index' as in
        output $x->where(sin($x) > 0.5),"\n";
             # and finally take the sin of these elements
             # (to show that these are indeed the correct ones)
	output sin($x->index($i)),"\n";
|;

comment q|
	We hope you enjoyed these demos illustrating some
	of the basic capabilities of PDL.

	We encourage you to play with these commands in
        the perldl or pdl2 shell and use its online help support
	to find out more about these and other commands and
	features of PDL.

        Just type 'help' to get started.

|;


}

1;
