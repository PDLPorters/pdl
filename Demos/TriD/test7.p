use blib;
use Carp;

$SIG{__DIE__} = sub {die Carp::longmess(@_);};

use PDL;
use PDL::Graphics::TriD;

$nx = 20;

$t =  (xvals zeroes $nx+1,$nx+1)/$nx;
$u =  (yvals zeroes $nx+1,$nx+1)/$nx;

$x = sin($u*15 + $t * 3)/2+0.5 + 5*($t-0.5)**2;

# Need to specify type first because points doesn't default to anything
points3d([SURF2D,$x]);
line3d([SURF2D,$x]);
mesh3d([$x]);
imag3d([$x],{Lines => 0});
imag3d([$x],{Lines => 0, Smooth => 1});
imag3d([$x]);

# Then, see the same image twice...

my $ox = $x->dummy(2,2)->zvals; # 0 for first, 1 for second surface.

imag3d([$x * ($ox-0.5) + $ox ],{Smooth => 1});
imag3d([$x * ($ox-0.5) + $ox ],{Lines => 0,Smooth => 1});
