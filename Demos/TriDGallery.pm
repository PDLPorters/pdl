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
        submit it to the PDL mailing list (perldl@jach.hawaii.edu)
        and there is a good chance it will soon be included in the gallery

	Press 'q' in the graphics window for the next screen.
	Rotate the image by pressing mouse button one and
	dragging in the graphics window.
	Zoom in/out by pressing MB3 and drag up/down.
|;

actnw q|
# B/W Mandelbrot... [Tjl]

use PDL; use PDL::Graphics::TriD;
$s=150;$a=zeroes $s,$s;$r=$a->xlinvals(-1.5,0.5);$i=$a->ylinvals(-1,1);
$t=$r;$u=$i;
for(0..12){$q=$r**2-$i**2+$t;$h=2*$r*$i+$u;($r,$i)=map{$_->clip(-5,5)}($q,$h);}
imagrgb[($r**2+$i**2)>2.0];

# [press 'q' in the graphics window when done]
|;

if(0) {
actnw q|
# Greyscale Mandelbrot [Tjl]

use PDL; use PDL::Graphics::TriD;$a=zeroes 300,300;$r=$a->xlinvals(-1.5,
0.5);$i=$a->ylinvals(-1,1);$t=$r;$u=$i;for(1..30){$q=$r**2-$i**2+$t;$h=2
*$r*$i+$u;$d=$r**2+$i**2;$a=lclip($a,$_*($d>2.0)*($a==0));($r,$i)=map{$_
->clip(-5,5)}($q,$h);}imagrgb[$a/30];

# [press 'q' in the graphics window when done]
|;

actnw q|
# Color Mandelbrot anim (nokeeptwiddling3d removed -> fits) [Tjl]

use PDL; use PDL::Graphics::TriD;
nokeeptwiddling3d();
$a=zeroes 300,300;$r=$a->xlinvals(-1.5,
0.5);$i=$a->ylinvals(-1,1);$t=$r;$u=$i;for(1..30){$q=$r**2-$i**2+$t;$h=2
*$r*$i+$u;$d=$r**2+$i**2;$a=lclip($a,$_*($d>2.0)*($a==0));($r,$i)=map{$_
->clip(-5,5)}$q,$h;imagrgb[($a==0)*($r/2+0.75),($a==0)*($i+1)/2,$a/30]}

# [press 'q' in the graphics window when done]
|;
}

if(0){
actnw q|
# Torus... (barrel) [Tjl]

use PDL; use PDL::Graphics::TriD;
$s=40;$a=zeroes $s,$s;$t=$a->xlinvals(0,6.284);
$u=$a->ylinvals(0,6.284);$o=5;$i=1;$v=$o+$i*sin$u;
imag3d([$v*sin$t,$v*cos$t,$i*cos$u]);
|;

actnw q|
# Ripply torus [Tjl]

use PDL; use PDL::Graphics::TriD;
$s=40;$a=zeroes 2*$s,$s/2;$t=$a->xlinvals(0,6.284);
$u=$a->ylinvals(0,6.284); $o=5;$i=1;$v=$o+$i*sin$u;
imag3d([$v*sin$t,$v*cos$t,$i*cos($u)+$o*sin(3*$t)]);
|;

actnw q|
# Ripply torus distorted [Tjl]

use PDL; use PDL::Graphics::TriD;
$s=40;$a=zeroes 2*$s,$s/2;$t=$a->xlinvals(0,6.284);$u=$a->ylinvals(0,
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
$b=zeroes(50,50,50);$b=sin(0.3*$b->rvals)*cos(0.3*$b->xvals);$c=0;
$a=byte($b>$c);foreach(1,2,4){$t=($a->slice("0:-2")<<$_);$t+=$a->slice("1:-1");
$a = $t->mv(0,2);} points3d [whichND(($a != 0) & ($a != 255))];
|;

actnw q|
# Lucy deconvolution (AJ 79, 745) [Robin Williams (=> TriD by Tjl)]
use PDL; use PDL::Graphics::TriD; nokeeptwiddling3d();
sub smth {use PDL::Image2D; conv2d($_[0],exp(-(rvals ones(3,3))**2));}
$a=rfits("m51.fits")->float; $c=$d=avg($a)+0*$a;
while(max $c>1.1) {$c=smth($a/smth($d));$d*=$c;imagrgb[$d/850];}
|;


# use PDL; use PDL::Image2D; use PDL::Graphics::TriD;nokeeptwiddling3d;
# $d=byte(random(zeroes(90,90))>0.5);do{$k=byte [[1,1,1],[1,0,1],[1,1,1]];
# imagrgb[$d]if($k++%2); $s=conv2d($d,$k)/8;$i=90*90*random(50);$t=$d->
# clump(2)-> index($i);$t.=($s->clump(2)->index($i)>.5);}while(!twiddle3d)

actnw q~
# Fractal mountain range [Tuomas Lukka]
use PDL;use PDL::Image2D;use PDL::Graphics::TriD; keeptwiddling3d(); $k=ones(5,5) / 25;
$a=5;$b=ones(1,1)/2;for(1..7){$c=$b->dummy(0,2)->clump(2)->xchg(0,1)->
dummy(0,2)->clump(2)->xchg(0,1)->copy;$c+=$a*$c->random;$a/=3;
$b=conv2d($c,$k); imag3d[$b],{Lines => 0}; }
~;

comment q|
	We hope you did like that and got a feeling of
        the power of PDL.

        Now it's up to you to submit even better TriD M4LSs.

|;

}

if(0) { # one possible addition to volume rendering...

use PDL; use PDL::Graphics::TriD;
$b=zeroes(50,50,50);$b=sin(0.3*$b->rvals)*cos(0.3*$b->xvals);$c=0;
$a=byte($b>$c);foreach(1,2,4){$t=($a->slice("0:-2")<<$_);$t+=$a->slice("1:-1");
$a = $t->mv(0,2);}points3d[map{$_+$_->float->random}whichND(($a!=0)&($a != 255))];

}

# Neat, but too big variation of color mandelbrot
if(0) {

use PDL; use PDL::Graphics::TriD;
nokeeptwiddling3d();
sub f {return abs(sin($_[0]*30))}
$a=zeroes 300,300;$r=$a->xlinvals(-1.5,
0.5);$i=$a->ylinvals(-1,1);$t=$r;$u=$i;for(1..30){$q=$r**2-$i**2+$t;$h=2
*$r*$i+$u;$d=$r**2+$i**2;$a=lclip($a,$_*($d>2.0)*($a==0));($r,$i)=map{$_
->clip(-5,5)}$q,$h;imagrgb[f(($a==0)*($r/2+0.75)),f(($a==0)*($i+1)/2),$a/30]}

}

1;
