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
$ENV{PGPLOT_DEV}=$^O =~ /MSWin32/ ? '/GW' : "/XSERVE";

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
    This demo illustrates the PDL::Transform module - in fact the
    examples shown are taken from the documentation (try
    'pdldoc Transform' to read it).

    It requires PGPLOT support in PDL and makes use of the image of
    m51 kindly provided by the IRAF group at the National Optical and
    Astronomical Observatories.

|;

act q|

    # ensure PDL::Transform is available
    use PDL::Transform;

    # read in the image ($m51path has been set up by this demo to
    # contain the location of the file)
    $m51 = rfits "$m51path/m51.fits";
  
    # we use a floating-point version in some of the demos 
    # to highlight the interpolation schemes
    $m51_fl = $m51->float;
    $m51_fl->sethdr($m51->gethdr)

|;

act q|

    # set up a simple linear scale
    $ts = t_linear( Scale => 3 );
    print $ts, "\n";

|;

act q|
    # Resampling with ->map and no FITS interpretation works in pixel space

    # display the original and transformed images
    $opt = { nx=>2, ny=>2, Charsize=>2 };
    $win = PDL::Graphics::PGPLOT::Window->new($opt);
    $just = { justify => 1 };
    $win->imag( $m51, $just );
    $win->label_axes("","","M51");

    # Shrink m51 by a factor of 3; origin is at lower left
    # (the "nofits" makes the resampling happen in pixel coordinate space)

    $win->imag( $ts->unmap($m51, {nofits=>1}), $just );
    $win->label_axes("","","M51 shrunk by 3");

    # Grow m51 by a factor of 3; origin still at lower left.

    $win->imag( $ts->map($m51, {nofits=>1}), $just );
    $win->label_axes("","","M51 grown by 3");

|;

act q|
    # To demonstrate working in scientific space, we can wrap the 
    # simple scale in the FITS pixel-to-scientific transformation:

    # set up the transform between scientific and pixel coordinates
    $tf = t_fits( $m51 );
    print $tf,"\n";

    # Make $ts happen in scientific coords:
    $t = t_wrap($ts,$tf);
    print $t,"\n";
|;

act q|
    # display the original and transformed images
    $win->panel(4); # this is so imag() appears in panel 1
    $win->imag( $m51, $just );
    $win->label_axes("","","M51");

    # Shrink m51 by a factor of 3; origin is at scientific origin.
    $win->imag( $t->unmap($m51_fl,{nofits=>1}), $just );
    $win->label_axes("","","M51 shrunk by 3");

    # Grow m51 by a factor of 3; origin is still at sci. origin.
    $win->imag( $t->map($m51_fl,{nofits=>1}), $just );
    $win->label_axes("","","M51 grown by 3 (interpolated)");

    # Grow m51 by a factor of 3; use sampling instead of bilinear interp.
    #  (much faster!)
    $win->imag( $t->map($m51_fl,{nofits=>1, method=>"sample"}) , $just );
    $win->label_axes("","","M51 grown by 3 (sampled)");
|;

act q|

    # Examine the radial structure in M51
    #
    # Scale to fill orig. image
    $ts = t_linear( s => pdl(250/2.0/3.14159, 2) );

    # Expand around galactic core
    $tu = t_radial( o => pdl(130,130) );

    # and repeat, using a conformal mapping
    #
    # Note scalar (heh) scale.
    $ts_c = t_linear( s=> 250/2.0/3.14159 );

    # 5 pix. radius -> bottom of image
    $tu_c = t_radial( o=> pdl(130,130), r0=>5 );

|;

# NOTE:
#   need to 'double protect' the \ in the label_axes()
#   since it's being evaluated twice (I think)
#
act q|

    # Original image
    $win->imag( $m51, $just );
    $win->label_axes("","","M51");

    # Stretched
    $win->imag( $ts->compose($tu)->map($m51_fl, [360,256],{nofits=>1}) );
    $win->label_axes("\\\\gh (degrees)","r (pixels)","M51 radial (linear)");

    # Conformal
    $win->panel(3);
    $win->imag( $ts_c->compose($tu_c)->map($m51_fl, [360,360],{nofits=>1}));
    $win->label_axes("\\\\gh (degrees)","r (log pixels)","M51 radial (conformal)");

|;

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
    $win->imag($m51,$just);
    $win->label_axes("","","M51");

    # Skewed
    $win->imag( $m51_fl->map( $t_skew, {nofits=>1}), $just);
    $win->label_axes("","","M51 skewed by \\\\gp in spatial coords");

    # Untwisted -- show that m51 has a half-twist per scale height
    $win->imag( $m51_fl->map( $t_untwist, {nofits=>1}),$just);
    $win->label_axes("","","M51 unspiraled (\\\\gp / r\\\\ds\\\\u)");

    # Untwisted -- the jacobean method uses variable spatial filtering 
    # to eliminate spatial artifacts, at significant computational cost
    # (This may take some time to complete).
    $win->imag( $m51_fl->map( $t_untwist, {nofits=>1, m=>jacobean}),$just);
    $win->label_axes("","","M51 unspiraled (\\\\gp / r\\\\ds\\\\u; antialiased)");
|;

act q|
    ##############################
    # This concludes the PDL::Transform demo.
    #
    # You can use transforms both to resample image data and to transform
    # vectors.  How to do that, along with many useful projection transforms,
    # is demonstrated in the "Cartography" demo.
    #
|;
} 

1;
