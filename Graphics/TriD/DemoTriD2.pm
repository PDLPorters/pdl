# Copyright (C) 1998 Tuomas J. Lukka.
# All rights reserved, except redistribution
# with PDL under the PDL License permitted.

package PDL::Demos::TriD2;

use PDL::Graphics::TriD;
use PDL::Graphics::TriD::Image;

sub info {('3d2', '3d demo, part 2. (Somewhat memory-intensive)')}
sub init {'
use PDL::Graphics::TriD;
'}

my @demo = (
[comment => q|
	Welcome to a short tour of the more esoteric capabilities of
	PDL::Graphics::TriD.

	As in '3d', press 'q' in the graphics window for the next
	screen.  Rotate the image by pressing mouse button one and
	dragging in the graphics window.

	Note that the script must start with

		use PDL;
		use PDL::Graphics::TriD;

	to work.
|],

[actnw => q|
	# See if we had a 3D window open already
	$|.__PACKAGE__.q|::we_opened = !defined $PDL::Graphics::TriD::current_window;

	# Number of subdivisions for lines / surfaces.
	$size = 25;

	$r = (xvals zeroes $size+1,$size+1) / $size;
	$g = (yvals zeroes $size+1,$size+1) / $size;
	$b = ((sin($r*6.3) * sin($g*6.3)) ** 3)/2 + 0.5;   # Bumps
	imagrgb [$r,$g,$b];	# Draw an image
        # [press 'q' in the graphics window when done]
|],

[actnw => q|
	# How about this?
	imagrgb3d([$r,$g,$b]);	# Draw an image on the lower plane
        # [press 'q' in the graphics window when done]
|],

[actnw => q|
	# Let's add the real image on top of this...
	hold3d();
	imag3d([$r,$g,$b+0.1], [$r,$g,$b]);
	# For the next demo, please rotate this so that much
	# of the image is visible.
	# Don't make your window too big or you might run out of memory
	# at the next step.
        # [press 'q' in the graphics window when done]
|],

[actnw => q|
	# Warning: your mileage will vary based on which
	# 	   OpenGL implementation you are using :(
	# Let's grab this picture...
	$pic = grabpic3d();

	# Lighten it up a bit so you see the background,
	# black on black is confusing
	$l = 0.3;
	sub lighten { $_[0] = ($_[0] + $l) / (1 + $l) }
	lighten($pic);

	# And plot it in the picture ;) ;)
	hold3d(); 	# You remember, we leave the previous one in...
	$o0 = imagrgb3d($pic, {Points => [[0,0,0],[0,1,0],[0,1,1],[0,0,1]]});

	# Because we have the data in $pic, we could just as easily
	# save it in a jpeg using the PDL::IO::Pic module - or read
	# it from one.
        # [press 'q' in the graphics window when done]
|],

[actnw => q|
	# That was fun - let's do that again!
	$pic1 = grabpic3d();

	lighten($pic1); # Lighten it up

	# And plot it in the picture ;) ;)
	hold3d(); 	# You remember, we leave the previous one in...
	$o1 = imagrgb3d($pic1, {Points => [[0,0,0],[1,0,0],[1,0,1],[0,0,1]]});
        # [press 'q' in the graphics window when done]
|],

[actnw => q|
	# Now, let's update them in real time!
	nokeeptwiddling3d(); # Don't wait for user while drawing
	while(1) {
		$p = grabpic3d();
		$p = ($p + $l) / (1 + $l);
		$pic .= $p; $pic1 .= $p;
		$_->data_changed for $o0, $o1;
		last if twiddle3d(); # exit from loop when 'q' pressed
	}
        # [press 'q' in the graphics window when done]
|],

[actnw => q|
      # Finally, leave 3d in a sane state
      keeptwiddling3d(); # Don't wait for user while drawing
      release3d();
      # close 3D window if we opened it
      close3d() if $|.__PACKAGE__.q|::we_opened;
|],
);

sub demo { @demo }

1;
