#
package PDL::Demos::Transform_demo;

use PDL;
use PDL::Graphics::PGPLOT::Window;
use PDL::Transform;

use File::Spec;

PDL::Demos::Routines->import();
sub comment($);
sub act($);
sub output;

sub run {
  local($PDL::debug) = 0;
  local($PDL::verbose) = 0;

##$ENV{PGPLOT_XW_WIDTH}=0.6;
$ENV{PGPLOT_DEV} = $^O =~ /MSWin32/          ? '/GW'            :
                   defined($ENV{PGPLOT_DEV}) ? $ENV{PGPLOT_DEV} : "/XWIN";

# try and find m51.fits
$d = File::Spec->catdir( "PDL", "Demos" );
$m51path = undef;
foreach my $path ( @INC ) {
    my $check = File::Spec->catdir( $path, $d );
    if ( -d $check ) { $m51path = $check; last; }
}
barf "Unable to find directory ${m51path} within the perl libraries.\n"
    unless defined $m51path;

comment q|
    This demo illustrates the PDL::Transform module.

    It requires PGPLOT support in PDL and makes use of the image of
    M51 kindly provided by the Hubble Heritage group at the 
    Space Telescope Science Institute.

|;

act q|
    # PDL::Transform objects embody coordinate transformations. 

    use PDL::Transform;

    # set up a simple linear scale-and-shift relation
  
    $t = t_linear( Scale=>[2,-1], Post=>[100,0]);
    print $t;
|;

act q|
    # The simplest way to use PDL::Transform is to transform a set of
    # vectors.  To do this you use the "apply" method.  

    # Define a few 2-vectors:
    $xy = pdl([[0,1],[1,2],[10,3]]);
    print "xy: ", $xy;

    # Transform the 2-vectors:
    print "Transformed: ", $xy->apply( $t );
|;

act q| 
    # You can invert and compose transformations with 'x' and '!'.
    $u = t_linear( Scale=>10 );     # A new transformation (simple x10 scale)
    $xy = pdl([[0,1],[10,3]]);      # Two 2-vectors
    print "xy:   ",  $xy;
    print "xy':  ",  $xy->apply( !$t     );     # Invert $t from earlier.
    print "xy'': ",  $xy->apply( $u x !$t );    # Hit the result with $u.
|;

act q|
    # PDL::Transform is useful for data resampling, and that's perhaps
    # the best way to demonstrate it.  First, we do a little bit of prep work:

    # Read in an image ($m51path has been set up by this demo to
    # contain the location of the file).  Transform is designed to 
    # work well with FITS images that contain WCS scientific coordinate
    # information, but works equally well in pixel space.

    $m51 = rfits("$m51path/m51.fits",{hdrcpy=>1});
  
    # we use a floating-point version of the image in some of the demos 
    # to highlight the interpolation schemes.  (Note that the FITS
    # header gets deep-copied automatically into the new variable).

    $m51_fl = $m51->float;

    # Define a nice, simple scale-by-3 transformation.

    $ts = t_scale(3);

|;

act q|
  #### Resampling with ->map and no FITS interpretation works in pixel space.

  ### Create a PGPLOT window, and display the original image
    $dev = $^O =~ /MSWin32/          ? '/GW'            :
           defined($ENV{PGPLOT_DEV}) ? $ENV{PGPLOT_DEV} : "/XW";
    $win = pgwin( dev=> $dev, nx=>2, ny=>2, Charsize=>2, J=>1, Size=>[8,6] );

    $win->imag( $m51 , { DrawWedge=>0, Title=>"M51" }  );

  ### Grow m51 by a factor of 3; origin is at lower left
  #   (the "pix" makes the resampling happen in pixel coordinate 
  #   space, ignoring the FITS header)

    $win->imag( $m51->map( $ts, {pix=>1} )  );
    $win->label_axes("","","M51 grown by 3 (pixel coords)");

  ### Shrink m51 by a factor of 3; origin still at lower left.
  #   (You can invert the transform with a leading '!'.)

    $win->imag( $m51->map( !$ts, {pix=>1} )  );
    $win->label_axes("","","M51 shrunk by 3 (pixel coords)");

|;

act q|
    # You can work in scientific space (or any other space) by 
    # wrapping your main transformation with something that translates
    # between the coordinates you want to act in, and the coordinates
    # you have.  Here, "t_fits" translates between pixels in the data
    # and arcminutes in the image plane.

    ### Clear the panel and start over

    $win->panel(4);                 # (Clear whole window on next plot)
    $win->imag( $m51, { Title=>"M51" } );

    ### Scale in scientific coordinates.
    #   Here's a way to scale in scientific coordinates:
    #   wrap our transformation in FITS-header transforms to translate
    #   the transformation into scientific space. 

    $win->imag(  $m51->map( !$ts->wrap(t_fits($m51)), {pix=>1} )   );
    $win->label_axes("","","M51 shrunk 3x (sci. coords)");

|;

act q|
 # If you don't specify "pix=>1" then the resampler works in scientific
 # FITS coordinates (if the image has a FITS header):

 ### Scale in scientific coordinates (origin at center of galaxy)
   $win->fits_imag( $m51->map( $ts, $m51->hdr ), { Title=>"M51 3x" } );

 ### Instead of setting up a coordinate transformation you can use the 
 #   implicit FITS header matching.  Just tweak the template header:
   $tohdr = $m51->hdr_copy;
   $tohdr->{CDELT1} /= 3;  # Magnify 3x in horiz direction
   $tohdr->{CDELT2} /= 3;  # Magnify 3x in vert direction
    
 ### Resample to match the new FITS header
 #   (Note that, although the image is scaled exactly the same as before,
 #   this time the scientific coordinates have scaled too.)
  $win->fits_imag( $m51->map( t_identity(), $tohdr ), { Title=>"3x (FITS)" } );
|;

act q|      
 ### The three main resampling methods are "sample", "linear", and "jacobian".
 #   Sampling is fastest, linear interpolation is better.  Jacobian resampling
 #   is slow but prevents aliasing under skew or reducing transformations.
  
 $win->fits_imag( $m51_fl , {Title=>"M51"} );

 $win->fits_imag( $m51_fl->map( $ts, $m51_fl, { method=>"sample" } ),
		{Title=>"M51 x3 (sampled)"} );

 $win->fits_imag( $m51_fl->map( $ts, $m51_fl, { method=>"linear" } ),
		{ Title=>"M51 x3 (interp.)"} );

 $win->fits_imag( $m51_fl->map( $ts, $m51_fl, { method=>"jacobian" } ),
 	        { Title=>"M51 x3 (jacob.)"} );

|;

act q|
 ### Linear transformations are only the beginning.  Here's an example 
 #  using a simple nonlinear transformation:  radial coordinate transformation.

 ### Original image
    $win->fits_imag( $m51 ,{Title=>"M51"});
  
 ### Radial structure in M51 (linear radial scale; origin at (0,0) by default)
    $tu = t_radial( u=>'degree' );
    $win->fits_imag( $m51_fl->map($tu), { Title=>"M51 radial (linear)", J=>0});

 ### Radial structure in M51 (conformal/logarithmic radial scale)
    $tu_c = t_radial( r0=>0.1 );  # Y axis 0 is at 0.1 arcmin
    $win->panel(3);
    $win->fits_imag( $m51_fl->map($tu_c), 
		     { Title=>"M51 radial (conformal)", 
 		       YRange=>[0,4] } );


|;
# NOTE:
#   need to 'double protect' the \ in the label_axes()
#   since it's being evaluated twice (I think)
#


act q|

    #####################
    # Wrapping transformations allows you to work in a convenient
    # space for what you want to do.  Here, we can use a simple
    # skew matrix to find (and remove) logarithmic spiral structures in 
    # the galaxy.  The "unspiraled" images shift the spiral arms into 
    # approximate straight lines.

    $sp = 3.14159;  # Skew by 3.14159 
  
    # Skew matrix
    $t_skew = t_linear(pre => [$sp * 130, 0] , matrix => pdl([1,0],[-$sp,1]));
    
    # When put into conformal radial space, the skew turns into 3.14159 
    # radians per scale height.
    $t_untwist = t_wrap($t_skew, $tu_c);

    # Press enter to see the result of these transforms...
|;

act q|
    ##############################
    # Note that you can use ->map and ->unmap as either PDL methods
    # or transform methods; what to do is clear from context.

    # Original image
    $win->fits_imag($m51, {Title => "M51"} );

    # Skewed
    $win->fits_imag( $m51_fl->map( $t_skew ),
	{ Title => "M51 skewed by \\\\gp in spatial coords" } );

    # Untwisted -- show that m51 has a half-twist per scale height
    $win->fits_imag( $m51_fl->map( $t_untwist ), 
	{ Title => "M51 unspiraled (\\\\gp / r\\\\ds\\\\u)"} );

    # Untwisted -- the jacobean method uses variable spatial filtering 
    # to eliminate spatial artifacts, at significant computational cost
    # (This may take some time to complete).
    $win->fits_imag( $m51_fl->map( $t_untwist, {m=>jacobean}),
        { Title => "M51 unspiraled (\\\\gp / r\\\\ds\\\\u; antialiased)" } );
|;


$win->close;

act q|
    ###   Native FITS interpretation makes it easy to view your data in
    ###   your preferred coordinate system.  Here we zoom in on a 0.2x0.2
    ###   arcmin region of M51, sampling it to 100x100 pixels resolution.
  
    $m51 = float $m51;
    $data = $m51->match([100,100],{or=>[[-0.05,0.15],[-0.05,0.15]]});
    $s = "M51 closeup ("; $ss=" coords)";
    $ps = " (pixels)";

    $dev = $^O =~ /MSWin32/          ? '/GW'            :
           defined($ENV{PGPLOT_DEV}) ? $ENV{PGPLOT_DEV} : "/XW";
    $w1 = pgwin( dev=> $dev, size=>[4,4], charsize=>1.5, justify=>1 );
    $w1->imag( $data, 600, 750, { title=>"${s}pixel${ss}", 
				  xtitle=>"X$ps", ytitle=>"Y$ps" } );
    $w1->hold;

    $w2 = pgwin( dev=> $dev, size=>[4,4], charsize=>1.5, justify=>1 );
    $w2->fits_imag( $data, 600, 750, { title=>"${s}sci.${ss}", dr=>0 } );
    $w2->hold;

    # Now please separate the two X windows on your screen, and press ENTER.
    ###############################
|;

act q|
    ###   Now rotate the image 360 degrees in 10 degree increments.
    ###   The 'match' method resamples $data to the rotated scientific
    ###   coordinate system in $hdr.  The "pixel coordinates" window shows 
    ###   the resampled data in their new pixel coordinate system. 
    ###   The "sci. coordinates" window shows the data remaining fixed in 
    ###   scientific space, even though the pixels that represent them are 
    ###   moving and rotating.

  $hdr = $data->hdr_copy;
  
  for( $rot=0; $rot<=360; $rot += 10 ) {
    $hdr->{CROTA2} = $rot;

    $d = $data->match($hdr);
    
    $w1->imag( $d, 600, 750 );
    $w2->fits_imag($d, 600, 750, {dr=>0});
  }
|;

act q|
   ###   You can do the same thing even with nonsquare coordinates.
   ###   Here, we resample the same region in scientific space into a 
   ###   150x50 pixel array.
   
  $data = $m51->match([150,50],{or=>[[-0.05,0.15],[-0.05,0.15]]});
  $hdr = $data->hdr_copy;

  $w1->release; 
  $w1->imag( $data, 600, 750, { title=>"${s}pixel${ss}", 
		                xtitle=>"X$ps", ytitle=>"Y$ps", pix=>1 } );
  $w1->hold;

  for( $rot=0; $rot<=750; $rot += 5 ) {
    $hdr->{CROTA2} = $rot;
    $d = $data->match($hdr);
    $w1->imag($d, 600, 750);    $w2->fits_imag($d, 600, 750, {dr=>0});
  }

  |;


comment q|

 This concludes the PDL::Transform demo.

 Be sure to check the documentation for PDL::Transform::Cartography,
 which contains common perspective and mapping coordinate systems
 that are useful for work on the terrestrial and celestial spheres,
 as well as other planets &c.

|;

  $w1->release; $w1->close; undef $w1;
  $w2->release; $w2->close; undef $w2;
  undef $win;
} 

1;
