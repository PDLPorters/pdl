# XXXX NOTHING BUT stupidpolygonize WORKS!!!!

package PDL::Graphics::TriD::StupidPolygonize;

use strict;
use warnings;
use PDL::Core '';

# A very simplistic polygonizer...
# Center = positive, outside = negative.

sub stupidpolygonize {
	my($center, $initrad, $npatches, $nrounds, $func) = @_;
	my $x = PDL->zeroes(PDL::float(),3,$npatches,$npatches);
	my $mult = 2*3.14 / ($npatches-1);
	my $ya = ($x->slice("(0)"))->xvals;
	$ya *= $mult;
	my $za = ($x->slice("(0)"))->yvals;
	$za *= $mult/2;
	$za -= 3.14/2;
	(my $tmp0 = $x->slice("(0)")) += cos($ya);
	(my $tmp1 = $x->slice("(1)")) += sin($ya);
	(my $tmp01 = $x->slice("0:1")) *= cos($za)->dummy(0);
	(my $tmp2 = $x->slice("(2)")) += sin($za);
	my $add = $x->copy;
	$x *= $initrad;
	$x += $center;
	my $cur = $initrad;
	my $inita = $x->copy;
	for(1..$nrounds) {
		$cur /= 2;
		my $vp = $func->($x);
		my $vps = ($vp > 0);
		$vps -= 0.5; $vps *= 2;
		$x += $vps->dummy(0) * $cur * $add;
	}
	return $x;
}

sub polygonizeraw {
	my($data,$coords) = @_;
}

sub contours {
}

package PDL::Graphics::TriD::ContourPolygonize;

#
# First compute contours.
use vars qw/$cube $cents/;
$cube = PDL->pdl([
 [-1,-1,-1],
 [-1,-1,1],
 [-1,1,-1],
 [-1,1,1],
 [1,-1,-1],
 [1,-1,1],
 [1,1,-1],
 [1,1,1]
]);

$cents = PDL->pdl([
 [0,0,-1],
 [0,0,1],
 [0,-1,0],
 [0,1,0],
 [-1,0,0],
 [1,0,0],
]);

sub contourpolygonize {
	my($in,$oscale,$scale,$func) = @_;
	my $ccube = $cube * $oscale;
	my $maxstep=0;
	while(($func->($ccube)>=0)->sum->sclr > 0) {
		$ccube *= 1.5;
		if($maxstep ++ > 30) {
			die("Too far inside");
		}
	}
# Now, we have a situation with a cube that has inside a I point
# and as corners O points. This does not guarantee that we have all
# the surface but it's enough for now.

}

#############

#sub trianglepolygonize {
#
#	find_3nn(
#}

package PDL::Graphics::TriD::Polygonize;

use PDL::Core '';

# Inside positive, outside negative!
#
# XXX DOESN'T WORK

sub polygonize {
	my($inv,$outv,$cubesize,$func) = @_;
	barf "Must be positive" if $cubesize <= 0;
	my $iv = $func->($inv);
	my $ov = $func->($outv);
	my $s;
# Find a close enough point to zero.
	while(((sqrt(($iv-$ov))**2))->sum->sclr > $cubesize) {
		my $s = $iv + $ov; $s /= 2;
		my $v = $func->($s);
		$v->sum->sclr < 0 ?
			$ov = $s :
			$iv = $s;
	}
# Correct the smaller distance to cubesize.
	$iv = $ov + ($iv-$ov) * $cubesize / sqrt(($iv-$ov)**2)
# If it went outside, do it the other way around.
#	if($func->($iv)->sum->sclr < 0) {
#		$ov = $iv + ($ov-$iv) * $cubesize / sqrt(($iv-$ov)**2)
#	}
# Now, |$iv-$ov| = $cubesize
#	Make the first cube


#	Then, start the cubes march.
}

# Cube coordinates.
sub marchcubes {
	my($init) = @_;

}

1;
