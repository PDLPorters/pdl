# Copyright (C) 1998 Tuomas J. Lukka.
# All rights reserved, except redistribution
# with PDL under the PDL License permitted.

package PDL::Demos::TriDGallery;

use PDL::Graphics::TriD;
use PDL::Graphics::TriD::Image;

sub info {('3dgal', 'the 3D gallery: make cool images with 3-line scripts')}
sub init {'
use PDL::Graphics::TriD;
use PDL::Graphics::TriD::Image;
'}

my @demo = (
[comment => q|
	Welcome to the TriD Gallery

	The following selection of scripts demonstrates that you
	can generate  interesting images with PDL (and the TriD
	modules) with just a few lines of code.
	Scripts accepted for this category:

        1) Must be legal Perl with a recent PDL version - may come with
           a patch to PDL if the patch is general enough to be included
           in the next release and usable outside the demo (e.g.
           $x=mandelbrot($c) is NOT), i.e. you can introduce new
           commands
        2) Must create an interesting image when fed to PDL

        If you have an interesting new TriD demo,
        submit it to the PDL mailing list (pdl-general@lists.sourceforge.net)
        or on a GitHub issue (https://github.com/PDLPorters/pdl/issues)
        and there is a good chance it will soon be included in the gallery

	Press 'q' in the graphics window for the next screen.
	Rotate the image by pressing mouse button one and
	dragging in the graphics window.
	Zoom in/out by pressing MB3 and drag up/down.
|],

[actnw => q|
$|.__PACKAGE__.q|::we_opened = !defined $PDL::Graphics::TriD::current_window;

# B/W Mandelbrot... [Tjl]

use PDL; use PDL::Graphics::TriD; # NOTE all demos need this, only showing once
$s=150;$x=zeroes $s,$s;$r=$x->xlinvals(-1.5,0.5);$i=$x->ylinvals(-1,1);
$t=$r;$u=$i;
for(0..12){$q=$r**2-$i**2+$t;$h=2*$r*$i+$u;($r,$i)=map{$_->clip(-5,5)}($q,$h);}
imagrgb[($r**2+$i**2)>2.0];

# [press 'q' in the graphics window when done]
|],

[actnw => q|
# Greyscale Mandelbrot [Tjl]

$x=zeroes 300,300; $r=$x->xlinvals(-1.5, 0.5);
$i=$x->ylinvals(-1,1); $t=$r; $u=$i;
for(1..30){
  $q=$r**2-$i**2+$t; $h=2*$r*$i+$u;
  $d=$r**2+$i**2; $x=lclip($x,$_*($d>2.0)*($x==0));
  ($r,$i)=map $_->clip(-5,5), $q, $h;
}
imagrgb[$x/30];

# [press 'q' in the graphics window when done]
|],

[actnw => q|
# Torus... (barrel) [Tjl]

$s=40;$x=zeroes $s,$s;$t=$x->xlinvals(0,6.284);
$u=$x->ylinvals(0,6.284);$o=5;$i=1;$v=$o+$i*sin$u;
imag3d([$v*sin$t,$v*cos$t,$i*cos$u]);

# [press 'q' in the graphics window when done]
|],

[actnw => q|
# Ripply torus [Tjl]

$s=40; $x=zeroes 2*$s,$s/2; $t=$x->xlinvals(0,6.284);
$u=$x->ylinvals(0,6.284); $o=5; $i=1; $v=$o+$i*sin $u;
imag3d([$v*sin$t,$v*cos$t,$i*cos($u)+$o*sin(3*$t)]);

# [press 'q' in the graphics window when done]
|],

[actnw => q|
# Ripply torus distorted [Tjl]

use PDL; use PDL::Graphics::TriD;
$s=40;$x=zeroes 2*$s,$s/2;$t=$x->xlinvals(0,6.284);$u=$x->ylinvals(0,
6.284); $o=5;$i=1;$v=$o-$o/2*sin(3*$t)+$i*sin$u;
imag3d([$v*sin$t,$v*cos$t,$i*cos($u)+$o*sin(3*$t)]);

# [press 'q' in the graphics window when done]
|],

[actnw => q|
# Volume rendering [Robin Williams]
$y=zeroes(50,50,50); $y=sin(0.3*$y->rvals)*cos(0.3*$y->xvals); $c=0;
$x=byte($y>$c);
foreach(1,2,4) {
  $t=($x->slice("0:-2")<<$_); $t+=$x->slice("1:-1"); $x = $t->mv(0,2);
}
points3d [whichND(($x != 0) & ($x != 255))->transpose->dog];

# [press 'q' in the graphics window when done]
|],

[actnw => q|
# one possible addition to volume rendering...
$y=zeroes(50,50,50); $y=sin(0.3*$y->rvals)*cos(0.3*$y->xvals); $c=0;
$x=byte($y>$c);
foreach (1,2,4) {
  $t= $x->slice("0:-2")<<$_;
  $t+=$x->slice("1:-1");
  $x = $t->mv(0,2);
}
points3d[map $_+$_->float->random, whichND(($x!=0)&($x != 255))->transpose->dog];
|],

# use PDL; use PDL::Image2D; use PDL::Graphics::TriD;nokeeptwiddling3d;
# $d=byte(random(zeroes(90,90))>0.5);do{$k=byte [[1,1,1],[1,0,1],[1,1,1]];
# imagrgb[$d]if($k++%2); $s=conv2d($d,$k)/8;$i=90*90*random(50);$t=$d->
# clump(2)-> index($i);$t.=($s->clump(2)->index($i)>.5);}while(!twiddle3d)

[actnw => q~
# Fractal mountain range [Tuomas Lukka]
use PDL::Image2D;
$k=ones(5,5) / 25; $x=5; $y=ones(1,1)/2;
sub PDL::stretchX { $_[0]->dummy(0,2)->clump(2) }
for(1..7) {
  $c=$y->stretchX->transpose->stretchX->transpose->copy;
  $c+=$x*$c->random; $x/=3;
  $y=conv2d($c,$k); imag3d[$y],{Lines => 0};
}

# [press 'q' in the graphics window to iterate (runs 7 times)]
~],

# act not actnw, and placed at end to work around keeptwiddling3d bug
[actnw => q~
# Electron simulation by Mark Baker: https://perlmonks.org/?node_id=963819
nokeeptwiddling3d;
$c = 0;
while (1) {
  $n = 6.28 * ++$c;
  $x = $c*rvals((zeros(9000))*$c);
  $cz = -1**$x*$c;
  $cy = -1**$x*sin$x*$c;
  $cx = -1**$c*rvals($x)*$c;
  $w = $cz-$cy-$cx;
  $g = sin($w);
  $r = cos($cy+$c+$cz);
  $b = cos($w);
  $i = ($cz-$cx-$cy);
  $q = $i*$n;
  points3d [ $b*sin($q), $r*cos($q), $g*sin$q], [$g,$b,$r];
  last if twiddle3d(); # exit from loop when 'q' pressed
}
keeptwiddling3d(); # restore waiting for user to press 'q'
~],

[actnw => q~
# Game of life [Robin Williams (edited by Tjl)]

use PDL::Image2D;
$d=byte(random(zeroes(40,40))>0.85); $k=byte [[1,1,1],[1,0,1],[1,1,1]];
nokeeptwiddling3d;
do {
  imagrgb [$d]; $s=conv2d($d,$k);
  $d&=($s<4); $d&=($s>1); $d|=($s==3);
} while (!twiddle3d);
keeptwiddling3d();

~],

[actnw => q|
# We hope you did like that and got a feeling of
# the power of PDL.

# Now it's up to you to submit even better TriD demos.
close3d() if $|.__PACKAGE__.q|::we_opened;
|],
);

sub demo { @demo }

my @disabled = (
[actnw => q|
# Color Mandelbrot anim (nokeeptwiddling3d removed -> fits) [Tjl]

use PDL; use PDL::Graphics::TriD;
nokeeptwiddling3d();
$x=zeroes 300,300;$r=$x->xlinvals(-1.5,
0.5);$i=$x->ylinvals(-1,1);$t=$r;$u=$i;
for(1..30) {
  $q=$r**2-$i**2+$t; $h=2*$r*$i+$u; $d=$r**2+$i**2;
  $x=lclip($x,$_*($d>2.0)*($x==0));
  ($r,$i)=map $_->clip(-5,5), $q,$h;
  imagrgb[($x==0)*($r/2+0.75),($x==0)*($i+1)/2,$x/30];
}

keeptwiddling3d();
# [press 'q' in the graphics window when done]
|],

[act => q|
# Lucy deconvolution (AJ 79, 745) [Robin Williams (=> TriD by Tjl)]
nokeeptwiddling3d();
sub smth {use PDL::Image2D; conv2d($_[0],exp(-(rvals ones(3,3))**2));}
$x=rfits("m51.fits")->float; $c=$d=avg($x)+0*$x;
while(max $c>1.1) {$c=smth($x/smth($d));$d*=$c;imagrgb[$d/850];}
keeptwiddling3d();

# [press 'q' in the graphics window when done]
|],

[actnw => q|
# spherical dynamics [Mark R Baker]
nokeeptwiddling3d();
for $c(1..99){
  $n=6.28*$c; $g=$c*rvals(sin(zeros(5000))*$c);
  $cz=-1**$g*$c; $cy=$g*cos$g*$c; $cx=$c*rvals($g)*$c;
  $g=cos($w=$cz+$cy+$cx); $r=sin$cy+$c+$cz; $y=sin$w;
  $i=$cz-$cx-$cy; $q=$i*$n;
  points3d[$y*sin$q,$r*cos$q,$g*sin$q],[$r,$g,$y];
}
keeptwiddling3d();

# [press 'q' in the graphics window when done]
|],

[actnw => q|
# Neat, but too big variation of color mandelbrot
sub f {return abs(sin($_[0]*30))}
$x=zeroes 300,300;
$r=$x->xlinvals(-1.5, 0.5); $i=$x->ylinvals(-1,1); $t=$r; $u=$i;
nokeeptwiddling3d();
for(1..30) {
  $q=$r**2-$i**2+$t;
  $h=2*$r*$i+$u; $d=$r**2+$i**2; $x=lclip($x,$_*($d>2.0)*($x==0));
  ($r,$i)=map $_->clip(-5,5), $q,$h;
  imagrgb[f(($x==0)*($r/2+0.75)),f(($x==0)*($i+1)/2),$x/30];
}
keeptwiddling3d();
|],

[actnw => q~
# Dewdney's voters (parallelized) [Tjl, inspired by the above 'life']

use PDL::Image2D;
nokeeptwiddling3d;
$d=byte(random(zeroes(100,100))>0.5);
do{
  $k=float [[1,1,1],[1,0,1],[1,1,1]];
  imagrgb[$d];
  $s=conv2d($d,$k)/8;
  $r = $s->float->random;
  $e = ($s>$r);
  $d .= $e;
} while(!twiddle3d);
keeptwiddling3d();
~],
);

1;
