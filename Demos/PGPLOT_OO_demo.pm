package PDL::Demos::PGPLOT_OO_demo;

# show how to use the new OO PGPLOT interface

use PDL;
use PDL::Graphics::PGPLOT::Window;

PDL::Demos::Routines->import();
sub comment($);
sub act($);
sub output;

sub run {

$ENV{PGPLOT_XW_WIDTH}=0.3;
$ENV{PGPLOT_DEV}=$^O =~ /MSWin32/ ? '/GW' : "/XSERVE";

comment q|
    The PGPLOT demo showed you how to use the old interface to PGPLOT.
    As this is perl, TIMTOWTDI, and this demo shows you how to use the
    new, object-orientated PGPLOT interface. For the simple examples
    shown here, the new method appears overkill; however, it really
    comes into its own when you wish to deal with multiple plots
    or windows.

    Enough prattle, on with the show...

|;

act q|
    # we start with a different module to the traditional interface
    use PDL::Graphics::PGPLOT::Window;

    # create a window "object"
    $dev = $^O =~ /MSWin32/ ? '/GW' : '/XSERVE';
    $win = PDL::Graphics::PGPLOT::Window->new( { Dev => $dev } );

|;

act q|
    # First we define some variables to use for the rest of the demo.
    $x=sequence(10);
    $y=2*$x**2;

    # Now a simple plot with points
    $win->points( $x, $y );

|;

act q|
    # Here is the same with lines
    $win->line( $x, $y );

    # if you're beginning to think its the same as the old calls, 
    # just with "$win->" at the beginning then you're not far wrong!

|;

act q|
    # You can do all the things you did before ...

    $win->points( $x, $y, {Symbol=>4} );
    $win->hold;
    $win->line( $x, $y );
    $yerr=sqrt($y);
    $win->errb( $x, $y, $yerr );

    $win->release;

|;

act q|
    # and it acts the same way

    $gradient=sequence(40,40);
    $win->imag( $gradient );
    $win->hold;
    $win->cont( $gradient );
    $win->release;

    # add labels to the plot
    $win->label_axes( "An axis", "Another axis", "Title" );
|;


act q|
  # let's try and read the cursor

  use PDL::Complex;
  $c =  zeroes(300)->xlinvals(0,12)+i*zeroes(300)->xlinvals(2,10);
  $sin = sin $c;
  $win->line( $sin->im, $sin->re );

  print "Select a point in the graph (mouse button or key press):\n";
  ( $x, $y, $ch ) = $win->cursor( { Type=>'CrossHair' } );
  
  print "\nYou selected: $x + $y i  (key = $ch)\n";

|;

# should really do something related to the selected points...

act q|
  # how about another window?

  $win2 = PDL::Graphics::PGPLOT::Window->new( { Dev => $dev } );
  $win2->env( 0, 4, -2, 0, { Axis => 'logy' } );
  $x = sequence(101) / 25;
  $win2->points( $x, $x->sin->abs()->log10 );

|;

act q|
  # switch back to the original window (we don't want to hurt
  # its feelings)

  $win->line( $x, { Border => 1 } );

|;

act q|
  # free up the windows, after finding their names

  print "You've been watching " . $win->name();
  print " and " . $win2->name() . "\n";

  $win->close();
  $win2->close();
    
|;

}

1;
