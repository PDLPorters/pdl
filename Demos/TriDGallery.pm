# Copyright (C) 1998 Tuomas J. Lukka.
# All rights reserved, except redistribution
# with PDL under the PDL License permitted.

package PDL::Demos::TriDGallery;

use PDL;
use PDL::Graphics::TriD;
use PDL::Graphics::TriD::Image;

PDL::Demos::Routines->import();
sub comment($);
sub act($);
sub actnw($);
sub output;

sub run {


comment q|
	Welcome to the TriD Gallery

	The following selection of scripts demonstrates that you
	can generate  interesting images with PDL (and the TriD
	modules) with just a few lines of code.

	These are the rules for scripts to be accepted for this
	category:


        1) Must be legal Perl with a recent PDL version - may come with
           a patch to PDL if the patch is general enough to be included
           in the next release and usable outside the demo (e.g.
           $x=mandelbrot($c) is NOT), i.e. you can introduce new
           commands

        2) The code must fit in 4 lines, 72 columns.

        3) It must create an interesting image when fed to perl.

        If you have an interesting new TriD M4LS (Maximal-4-lines-script)
        submit it to the PDL mailing list (pdl-general@lists.sourceforge.net)
        and there is a good chance it will soon be included in the gallery

	Press 'q' in the graphics window for the next screen.
	Rotate the image by pressing mouse button one and
	dragging in the graphics window.
	Zoom in/out by pressing MB3 and drag up/down.
|;

actnw q|
# B/W Mandelbrot... [Tjl]

use PDL; use PDL::Graphics::TriD;
$s=150;$x=zeroes $s,$s;$r=$x->xlinvals(-1.5,0.5);$i=$x->ylinvals(-1,1);
$t=$r;$u=$i;
for(0..12){$q=$r**2-$i**2+$t;$h=2*$r*$i+$u;($r,$i)=map{$_->clip(-5,5)}($q,$h);}
imagrgb[($r**2+$i**2)>2.0];

# [press 'q' in the graphics window when done]
|;

if(0) {
actnw q|
# Greyscale Mandelbrot [Tjl]

use PDL; use PDL::Graphics::TriD;$x=zeroes 300,300;$r=$x->xlinvals(-1.5,
0.5);$i=$x->ylinvals(-1,1);$t=$r;$u=$i;for(1..30){$q=$r**2-$i**2+$t;$h=2
*$r*$i+$u;$d=$r**2+$i**2;$x=lclip($x,$_*($d>2.0)*($x==0));($r,$i)=map{$_
->clip(-5,5)}($q,$h);}imagrgb[$x/30];

# [press 'q' in the graphics window when done]
|;

actnw q|
# Color Mandelbrot anim (nokeeptwiddling3d removed -> fits) [Tjl]

use PDL; use PDL::Graphics::TriD;
nokeeptwiddling3d();
$x=zeroes 300,300;$r=$x->xlinvals(-1.5,
0.5);$i=$x->ylinvals(-1,1);$t=$r;$u=$i;for(1..30){$q=$r**2-$i**2+$t;$h=2
*$r*$i+$u;$d=$r**2+$i**2;$x=lclip($x,$_*($d>2.0)*($x==0));($r,$i)=map{$_
->clip(-5,5)}$q,$h;imagrgb[($x==0)*($r/2+0.75),($x==0)*($i+1)/2,$x/30]}

# [press 'q' in the graphics window when done]
|;
}

if(0){
actnw q|
# Torus... (barrel) [Tjl]

use PDL; use PDL::Graphics::TriD;
$s=40;$x=zeroes $s,$s;$t=$x->xlinvals(0,6.284);
$u=$x->ylinvals(0,6.284);$o=5;$i=1;$v=$o+$i*sin$u;
imag3d([$v*sin$t,$v*cos$t,$i*cos$u]);
|;

actnw q|
# Ripply torus [Tjl]

use PDL; use PDL::Graphics::TriD;
$s=40;$x=zeroes 2*$s,$s/2;$t=$x->xlinvals(0,6.284);
$u=$x->ylinvals(0,6.284); $o=5;$i=1;$v=$o+$i*sin$u;
imag3d([$v*sin$t,$v*cos$t,$i*cos($u)+$o*sin(3*$t)]);
|;

actnw q|
# Ripply torus distorted [Tjl]

use PDL; use PDL::Graphics::TriD;
$s=40;$x=zeroes 2*$s,$s/2;$t=$x->xlinvals(0,6.284);$u=$x->ylinvals(0,
6.284); $o=5;$i=1;$v=$o-$o/2*sin(3*$t)+$i*sin$u;
imag3d([$v*sin$t,$v*cos$t,$i*cos($u)+$o*sin(3*$t)]);
|;

actnw q~
# Game of life [Robin Williams (edited by Tjl)]

use PDL; use PDL::Image2D; use PDL::Graphics::TriD;nokeeptwiddling3d;
$d=byte(random(zeroes(40,40))>0.85);$k=byte [[1,1,1],[1,0,1],[1,1,1]];
do{ imagrgb [$d]; $s=conv2d($d,$k);
$d&=($s<4);$d&=($s>1);$d|=($s==3);} while (!twiddle3d);

~;

actnw q~
# Dewdney's voters (parallelized) [Tjl, inspired by the above 'life']

use PDL; use PDL::Image2D; use PDL::Graphics::TriD;nokeeptwiddling3d;$d=
byte(random(zeroes(100,100))>0.5);do{$k=float [[1,1,1],[1,0,1],[1,1,1]];
imagrgb[$d]; $s=conv2d($d,$k)/8; $r = $s->float->random;
$e = ($s>$r); $d .= $e; }while(!twiddle3d)

~;
}

actnw q|
# Volume rendering [Robin Williams]
use PDL; use PDL::Graphics::TriD; keeptwiddling3d();
$y=zeroes(50,50,50);$y=sin(0.3*$y->rvals)*cos(0.3*$y->xvals);$c=0;
$x=byte($y>$c);foreach(1,2,4){$t=($x->slice("0:-2")<<$_);$t+=$x->slice("1:-1");
$x = $t->mv(0,2);} points3d [whichND(($x != 0) & ($x != 255))];
|;

actnw q|
# Lucy deconvolution (AJ 79, 745) [Robin Williams (=> TriD by Tjl)]
use PDL; use PDL::Graphics::TriD; nokeeptwiddling3d();
sub smth {use PDL::Image2D; conv2d($_[0],exp(-(rvals ones(3,3))**2));}
$x=rfits("m51.fits")->float; $c=$d=avg($x)+0*$x;
while(max $c>1.1) {$c=smth($x/smth($d));$d*=$c;imagrgb[$d/850];}
|;


# use PDL; use PDL::Image2D; use PDL::Graphics::TriD;nokeeptwiddling3d;
# $d=byte(random(zeroes(90,90))>0.5);do{$k=byte [[1,1,1],[1,0,1],[1,1,1]];
# imagrgb[$d]if($k++%2); $s=conv2d($d,$k)/8;$i=90*90*random(50);$t=$d->
# clump(2)-> index($i);$t.=($s->clump(2)->index($i)>.5);}while(!twiddle3d)

actnw q|
# spherical dynamics [Mark R Baker]
use PDL;use PDL::Graphics::TriD;for $c(1..99){$n=6.28*$c; $g=$c*rvals(
sin(zeros(5000))*$c);$cz=-1**$g*$c;$cy=$g*cos$g*$c;$cx=$c*rvals($g)*$c;
$g=cos($w=$cz+$cy+$cx);$r=sin$cy+$c+$cz;$y=sin$w;nokeeptwiddling3d();
$i=$cz-$cx-$cy;$q=$i*$n;points3d[$y*sin$q,$r*cos$q,$g*sin$q],[$r,$g,$y]}
|;

actnw q~
# Fractal mountain range [Tuomas Lukka]
use PDL;use PDL::Image2D;use PDL::Graphics::TriD; keeptwiddling3d(); $k=ones(5,5) / 25;
$x=5;$y=ones(1,1)/2;for(1..7){$c=$y->dummy(0,2)->clump(2)->xchg(0,1)->
dummy(0,2)->clump(2)->xchg(0,1)->copy;$c+=$x*$c->random;$x/=3;
$y=conv2d($c,$k); imag3d[$y],{Lines => 0}; }
~;

comment q|
	We hope you did like that and got a feeling of
        the power of PDL.

        Now it's up to you to submit even better TriD M4LSs.

|;

}

if(0) { # one possible addition to volume rendering...

use PDL; use PDL::Graphics::TriD;
$y=zeroes(50,50,50);$y=sin(0.3*$y->rvals)*cos(0.3*$y->xvals);$c=0;
$x=byte($y>$c);foreach(1,2,4){$t=($x->slice("0:-2")<<$_);$t+=$x->slice("1:-1");
$x = $t->mv(0,2);}points3d[map{$_+$_->float->random}whichND(($x!=0)&($x != 255))];

}

# Neat, but too big variation of color mandelbrot
if(0) {

use PDL; use PDL::Graphics::TriD;
nokeeptwiddling3d();
sub f {return abs(sin($_[0]*30))}
$x=zeroes 300,300;$r=$x->xlinvals(-1.5,
0.5);$i=$x->ylinvals(-1,1);$t=$r;$u=$i;for(1..30){$q=$r**2-$i**2+$t;$h=2
*$r*$i+$u;$d=$r**2+$i**2;$x=lclip($x,$_*($d>2.0)*($x==0));($r,$i)=map{$_
->clip(-5,5)}$q,$h;imagrgb[f(($x==0)*($r/2+0.75)),f(($x==0)*($i+1)/2),$x/30]}

}
1;
