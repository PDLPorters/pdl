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
    $m51_float = $m51->float;

|;

act q|

    # read the pixel transformation info from the FITS header
    $tf = t_fits($m51);
    print $tf, "\n";

|;

act q|

    # set up a simple linear scale
    $ts = t_linear( Scale => 3 );
    print $ts, "\n";

|;

act q|

    # display the original and transformed images
    $opt = { nx=>2, ny=>2, Charsize=>2 };
    $win = PDL::Graphics::PGPLOT::Window->new($opt);
    $just = { justify => 1 };
    $win->imag( $m51, $just );
    $win->label_axes("","","M51");

    # Shrink m51 by a factor of 3; origin is at lower left
    $win->imag( $ts->unmap($m51), $just );
    $win->label_axes("","","M51 shrunk by 3");

    # Grow m51 by a factor of 3; origin still at lower left.
    $win->imag( $ts->map($m51), $just );
    $win->label_axes("","","M51 grown by 3");

|;

act q|

    # Move the transform into FITS scientific space;
    #   $t gets ($tf^-1 o $ts o $tf).
    $t = $ts->wrap($tf);
    print $t, "\n";

|;

act q|
    # display the original and transformed images
    $win->panel(4); # this is so imag() appears in panel 1
    $win->imag( $m51, $just );
    $win->label_axes("","","M51");

    # Shrink m51 by a factor of 3; origin is at scientific origin.
    $win->imag( $t->unmap($m51_float), $just );
    $win->label_axes("","","M51 shrunk by 3");

    # Grow m51 by a factor of 3; origin is still at sci. origin.
    $win->imag( $t->map($m51_float), $just );
    $win->label_axes("","","M51 grown by 3 (slow)");

    # Grow m51 by a factor of 3; use sampling instead of bilinear interp.
    #  (much faster!)
    $win->imag( $t->map($m51_float,{method=>"sample"}) );
    $win->label_axes("","","M51 grown by 3 (fast)");

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
    $win->imag( $m51 );
    $win->label_axes("","","M51");

    # Stretched
    $win->imag( $ts->compose($tu)->map($m51_float) );
    $win->label_axes("\\\\gh","r","M51");

    # Conformal
    $win->panel(3);
    $win->imag( $ts_c->compose($tu_c)->map($m51_float) );
    $win->label_axes("\\\\gh","r","M51 (conformal)");

    $win->close();

|;

} 

1;
