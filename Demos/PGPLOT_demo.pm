package PDL::Demos::PGPLOT_demo;
use PDL;
use PDL::Graphics::PGPLOT;

PDL::Demos::Routines->import();
sub comment($);
sub act($);
sub output;

sub run {

$ENV{PGPLOT_XW_WIDTH}=0.3;
$ENV{PGPLOT_DEV}=$^O =~ /MSWin32/ ? '/GW' : "/XSERVE";

comment q|
    Welcome to this tour of the PDL's PGPLOT interface.

    This tour will introduce the PDL's PGPLOT plotting module and show
    what this powerful package can provide in terms of plotting. It is
    not designed to give a full tour of PGPLOT, you are advised to see
    the routines provided with pgperl for that.

    The PDL::Graphics::PGPLOT module provides a high-level interface
    to PGPLOT. However if you want even better control of your plots 
    you might want to include the PGPLOT module specifically:

       use PGPLOT;

    One aspect of PGPLOT that requires mention is the use of devices:
    Normally PGPLOT will inquire you about what device you want to use,
    with the prompt:

        Graphics device/type (? to see list, default /NULL):


|;

act q|
    # ensure the module is loaded (required for PDL versions >= 2.004)
    use PDL::Graphics::PGPLOT;
    # The size of the window can be specified
    $ENV{PGPLOT_XW_WIDTH}=0.3;
    # You can set your device explicitly
    dev($^O =~ /MSWin32/ ? '/GW' : '/XSERVE');
|;

act q|
    # First we define some variables to use for the rest of the demo.
    $x=sequence(10);
    $y=2*$x**2;

    # Now a simple plot with points
    points $x, $y;
|;

act q|
    # Here is the same with lines
    line $x, $y;
|;

act q|
    # If you want to overlay one plot you can use the command
    # 'hold' to put the graphics on hold and 'release' to
    # revert the effect

    points $x, $y, {SYMBOL=>4};  # The last argument sets symboltype
    hold;
    # Now draw lines between the points
    line $x, $y;
    # Plot errorbars over the points
    $yerr=sqrt($y);
    errb $x, $y, $yerr;

    # To revert to old behaviour, use release
    release;
|;

act q|
    bin $x, $y;

    # This plots a binned histogram of the data and as you can
    # see it made a new plot.
|;

act q|
    # 2D data can also easily be accomodated:

    # First make a simple image
    $gradient=sequence(40,40);

    # Then display it.
    imag $gradient;

    # And overlay a contour plot over it:
    hold;
    cont $gradient;
    release;
|;

act q|
  # PDL::Graphics::PGPLOT contains several colour tables,
  # a more extensive collection can be found in 
  # PDL::Graphics::LUT
  #
  # (note: the call to lut_names() can take a few seconds to execute)
  #
  use PDL::Graphics::LUT;
  @names = lut_names();
  print "Available tables: [ ", @names, " ]\n";

  # use the first table
  ctab( lut_data($names[0]) );
  use PGPLOT;
  pglabel "", "", "Colour table: $names[0]";

|;

act q|
    # To change plot specifics you can either use the specific PGPLOT
    # commands - recommended if you need lots of control over your
    # plot.
    #
    # Or you can use the new option specifications:

    # To plot our first graph again with blue color, dashed line
    # and a thickness of 10 we can do:

    line $x, $y, {COLOR=>5, LINESTYLE=>'dashed', LINEWIDTH=>10};

|;

act q|

  # Now for a more complicated example.
  # First create some data
  $a=sequence(360)*3.1415/180.;
  $b=sin($a)*transpose(cos($a));

  # Make a piddle with the wanted contours
  $contours=pdl [0.1,0.5,1.0];
  # And an array (reference to an array) with labels
  $labels=['A', 'B', 'C'];
  # Create a contour map of the data - note that we can set the colour of
  # the labels.
  cont($b, {CONTOURS=>$contours, linest=>'DASHED',
	    LINEWIDTH=>3, COLOR=>2, LABELCOL=>4});
  hold;

  pgqlw($linewidth);

  points $a->slice('0:-1:4')*180./3.1415;
  release;
|;

act q|
  #
  # More examples of changing the plot defaults
  # 
  $a = 1+sequence(10);
  $b = $a*2;
  $bord_opt = { TYPE => 'RELATIVE', VALUE => 0.1 };
  line log10($a), $b, { AXIS => 'LOGX', BORDER => $bord_opt };
|;

act q|
  #
  # We can also create vector maps of data
  # This requires one array for the horizontal component and
  # one for the vertical component
  #
  $horizontal=sequence(10,10);
  $vertical=transpose($horizontal)+random(10,10)*$horizontal/10.;

  $arrow={ARROW=> {FS=>1, ANGLE=>25, VENT=>0.7, SIZE=>3}};
  vect $horizontal, $vertical, {ARROW=>$arrow, COLOR=>RED};
  hold;
  cont $vertical-$horizontal, {COLOR=>YELLOW};
  release;

|;

act q|
  #
  # To draw [filled] polygons, the command poly is handy:
  #

  $x=sequence(10)/5;
  poly $x, $x**2, {FILL=>HATCHED, COLOR=>BLUE};

|;

act q|
  #
  # the latest feature of PDL are complex numbers
  # so let's play with a simple example
  #
  

  use PDL::Complex;
  $z50 = zeroes(50);
  $c = $z50->xlinvals(0,7)+i*$z50->xlinvals(2,4);
  line im sin $c; hold;      # the imaginary part
  line re sin $c;            # real
  line abs sin $c; release;  # and the modulus
  
|;

act q|
  #
  # more complex numbers
  #
  
  use PDL::Complex;
  $c =  zeroes(300)->xlinvals(0,12)+i*zeroes(300)->xlinvals(2,10);
  $sin = sin $c;
  line $sin->im, $sin->re;   # look at the result in the complex plane
  
|;

}

1;
