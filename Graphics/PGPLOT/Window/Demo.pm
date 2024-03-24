package PDL::Demos::PGPLOT_OO;

# show how to use the new OO PGPLOT interface

use PDL::Graphics::PGPLOT::Window;
require File::Spec;
use Carp;

$ENV{PGPLOT_XW_WIDTH}=0.3;
$ENV{PGPLOT_DEV}=$^O =~ /MSWin32/ ? '/GW' : "/XW";

sub info {('pgplotOO', 'PGPLOT OO interface')}
sub init {'
use PDL::Graphics::PGPLOT::Window;
'}

# try and find m51.fits
my @f = qw(PDL Demos m51.fits);
our $m51file = undef;
foreach my $path ( @INC ) {
    my $file = File::Spec->catfile( $path, @f );
    if ( -f $file ) { $m51file = $file; last; }
}
confess "Unable to find m51.fits within the perl libraries.\n"
    unless defined $m51file;

my @demo = (
[comment => q|
    The PGPLOT demo showed you how to use the old interface to PGPLOT.
    As this is perl, TIMTOWTDI, and this demo shows you how to use the
    new, object-orientated PGPLOT interface. For the simple examples
    shown here, the new method appears overkill; however, it really
    comes into its own when you wish to deal with multiple plots
    or windows.

    Enough prattle, on with the show...
|],

[act => q|
    # we start with a different module to the traditional interface
    use PDL::Graphics::PGPLOT::Window;

    # create a window "object"
    $dev = $ENV{PGPLOT_DEV}; # '/XW' on X, '/GW' on Win32
    $win = PDL::Graphics::PGPLOT::Window->new( { Dev => $dev } );
|],

[act => q|
    # First we define some variables to use for the rest of the demo.
    $x=sequence(10);
    $y=2*$x**2;

    # Now a simple plot with points
    $win->points( $x, $y );
|],

[act => q|
    # Here is the same with lines
    $win->line( $x, $y );

    # if you're beginning to think its the same as the old calls, 
    # just with "$win->" at the beginning then you're not far wrong!
|],

[act => q|
    # A ::Window-only feature: gaps in lines with MISSING option
    #   (the value is for the $y):
    $win->line( $x, $y, {MISSING => 8} );
|],

[act => q|
    # You can do all the things you did before ...

    $win->points( $x, $y, {Symbol=>4} );
    $win->hold;
    $win->line( $x, $y );
    $yerr=sqrt($y);
    $win->errb( $x, $y, $yerr );

    $win->release;
|],

[act => q|
    # and it acts the same way

    $gradient=sequence(40,40);
    $win->imag( $gradient );
    $win->hold;
    $win->cont( $gradient );
    $win->release;

    # add labels to the plot
    $win->label_axes( "An axis", "Another axis", "Title" );
|],

[act => q|
  # let's try and read the cursor

  $c =  czip(zeroes(300)->xlinvals(0,12), zeroes(300)->xlinvals(2,10));
  $sin = sin $c;
  $win->line( $sin->im, $sin->re );

  print "Select a point in the graph (mouse button or key press):\n";
  ( $x, $y, $ch ) = $win->cursor( { Type=>'CrossHair' } );
  
  print "\nYou selected: $x + $y i  (key = $ch)\n";
|],

# should really do something related to the selected points...

[act => q|
  # how about another window?

  $win2 = PDL::Graphics::PGPLOT::Window->new( { Dev => $dev } );
  $win2->env( 0, 4, -2, 0, { Axis => 'logy' } );
  $x = sequence(101) / 25;
  $win2->points( $x, $x->sin->abs()->log10 );
|],

[act => q|
  # switch back to the original window (we don't want to hurt
  # its feelings)

  $win->line( $x, { Border => 1 } );
|],

[act => q|
  # Read in an image ($m51file has been set up by this demo to
  # contain the location of the file).
  $m51 = rfits($|.__PACKAGE__.q|::m51file);
  $win3 = PDL::Graphics::PGPLOT::Window->new(Dev => $dev, Size=> [6,4],
    NX=>2, NY=>2, Ch=>2.5, HardCH=>2.5);
  $win3->imag($m51,{Title=>"\$win3->imag(\$m51);"} );
  $win3->fits_imag($m51,{Title=>"\$win3->fits_imag(\$m51);"});
  $win3->imag($m51,{J=>1,Title=>"\$win3->imag(\$m51,{J=>1});"});
  $win3->fits_imag($m51,{J=>1,Title=>"\$win3->fits_imag(\$m51,{J=>1});"});

  # You should see a 6 inch (153 mm) x 4 inch (102 mm) X window with four
  # plots in it. All four images should have tick marks on the outside of
  # the axes.

  # [ Scaled image of m51; scale   [Scaled image of m51 with scale from
  #   in pixels on both axes ]      X=[-1.8, 2.0],Y=[-1.9, 1.9] arcmin,
  #                                 with cal. wedge, centered in rect. frame]

  # [ Square image of m51; scale   [Square image of m51 with scale as above,
  #   in pixels on both axes;       ``shrink-wrapped'']
  #   ``shrinkwrapped'' ]
|],

[act => q|
  # free up the windows, after finding their names

  print "You've been watching ", $win->name, ", ", $win2->name, "\n";
  print " and ", $win3->name, "\n";

  $win->close; undef $win;
  $win2->close; undef $win2;
  $win3->release; $win3->close; undef $win3;

  print "On X Windows, you need to close the 'PGPLOT Server' window.\n";
|],
);

sub demo { @demo }

1;

=head1 NAME

PDL::Demos::PGPLOT_OO - demonstrate PDL::Graphics::PGPLOT OO capabilities

=head1 SYNOPSIS

  pdl> demo pgplotOO

=cut
