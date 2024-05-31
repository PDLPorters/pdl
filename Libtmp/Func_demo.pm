package PDL::Demos::Func_demo;

use PDL::Graphics::Simple;
use PDL::Func;
use PDL::Slatec; # no PCHIP without it

sub info {('func', 'Interpolation etc (Req.: PDL::Graphics::Simple)')}

sub init {'
use PDL::Graphics::Simple;
'}

my @demo = (
[act => q|
  # This demo illustrates the PDL::Func module.
  # PDL::Func objects encapsulate data to interpolate, integrate,
  # and get gradients of (differentiate).

  use PDL::Func qw(pchip spline); # load, and import convenience functions
  $w = pgswin(); # PDL::Graphics::Simple window
|],

[act => q|
  # set up a step function, similar to
  # https://uk.mathworks.com/help/matlab/ref/pchip.html
  $x = sequence(7) - 3;
  $y = pdl q[-1 -1 -1 0 1 1 1];
  # The convenience function "pchip" uses SLATEC's PCHIP with all
  # the default settings
  $xi = zeroes(100)->xlinvals(-3,3);
  $yi = pchip($x, $y, $xi);
  $yi_s = spline($x, $y, $xi);
  $w->plot(with => 'line', key => 'spline', $xi, $yi_s,
    with => 'line', key => 'pchip', $xi, $yi,
    with => 'points', $x, $y,
    {legend=>'tl'});
|],

[act => q|
  # Now a more undulating function, where PCHIP is less effective
  $x2 = sequence(16);
  $y2 = bessj1($x2);
  $xi2 = zeroes(100)->xlinvals(0,15);
  $yi2 = pchip($x2, $y2, $xi2);
  $yi2_s = spline($x2, $y2, $xi2);
  $w->plot(with => 'line', key => 'spline', $xi2, $yi2_s,
    with => 'line', key => 'pchip', $xi2, $yi2,
    with => 'points', $x2, $y2,
    {legend=>'tr'});
|],

[act => q|
  # And because it's PDL, it can broadcast seamlessly
  $y3 = cat( $x2*$x2+43.3, $x2*$x2*$x2-23 ); # dim 16,2
  $yi3 = pchip($x2, $y3, $xi2);
  # even though PDL::Graphics::Simple can't (yet)
  my @y3d = $y3->dog;
  my @yi3d = $yi3->dog;
  $w->plot(with => 'points', $x2, $y3d[0],
    with => 'points', $x2, $y3d[1],
    with => 'line', $xi2, $yi3d[0],
    with => 'line', $xi2, $yi3d[1]);
|],

[comment => q|
 This concludes the PDL::Func demo.

 Be sure to check the documentation for PDL::Func, to see further
 possibilities.
|],
);

sub demo { @demo }
sub done {'
  undef $w;
'}

1;
