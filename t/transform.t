use PDL::LiteF;
use Test;
use warnings;

BEGIN {
  plan tests => 27;
}

use PDL::Transform;

##############################
##############################
# Test basic transformation
$t = t_linear(scale=>[2]);
ok( $t->{idim} == 1 && $t->{odim} == 1, 1, "t_linear can make a 1-d transform" );

$a = sequence(2,2)+1;
$b = $a->apply($t);

ok( all( approx( $b, pdl( [2, 2], [6, 4] ) )), 1, "1-d apply on a collection of vectors ignors higher dim");

$t2 = t_linear(scale=>[2,3]);

ok( $t2->{idim} == 2 && $t2->{odim} == 2, 1, "t_linear can make a 2-d transform" );

$b = $a->apply($t2);

ok( all( approx( $b, pdl( [2, 6], [6, 12] ) )), 1, "2-d apply treats the higher dim");

$b = pdl(2,3)->invert($t2);
ok( all( approx($b, 1) ), 1, "invert works");




##############################
# Simple testing of the map autoscaling

$a = sequence(5,5);

# Identity transformation should be an expensive no-op
# (autoscaled correctly)
$b = $a->map(t_identity());
ok( all($a==$b) );

# Identity transformation on pixels should be a slightly less expensive
# no-op (no autoscaling)
$b = $a->map(t_identity,{pix=>1});
ok( all($a==$b) );

# Scaling by 2 and then autoscaling should be an expensive no-op
# (scaled, then autoscaled back down)
$b = $a->map(t_scale(2));
ok( all($a==$b) );

# Scaling by 2 in pixel coordinates should actually scale the image
$b = $a->map(t_scale(2),{pix=>1});
ok(all($b == $a*0.5));

##############################
# diab jerius' t_scale crash
# (this is due to a problem with inplace flag handling in PDL <= 2.6; transform works around it)

$a = pdl(49,49);
$t = t_linear({scale=>pdl([1,3]), offset=>pdl([12,8])});
$b = pdl( double, 2.2, 9.3);
$a->inplace->apply($t);
$q = 0;
$a += $q;
ok(1);  # still here!

##############################
# bad value handling...

if($PDL::Bad::Status) {
    $a = sequence(5,5);
    no warnings;
    $t1 = t_linear(pre=>[1.5,2]);
    $t2 = t_linear(pre=>[1,2]);
    use warnings;

    $a->badflag(1);

    eval q{$b = $a->map($t1,{pix=>1,method=>'l'});};
    ok(!$@);

    ok(($b->slice("0:1")->isbad->all  and  $b->slice(":,0:1")->isbad->all  and ($b->isbad->sum==16)), 1,"Bad values happen");

    eval q{$b = $a->map($t1,{pix=>1,method=>'h'});};
    ok(($b->slice("0")->isbad->all  and  $b->slice(":,0:1")->isbad->all and $b->isbad->sum==13), 1,"Bad values happen with 'h' method"); 
    

} else {
    skip(3, "Bad value support not included");
}

use PDL::IO::FITS;
$m51 = rfits('m51.fits');
$m51map = $m51->map(t_identity,{method=>'s'}); #SHOULD be a no-op
ok(all($m51==$m51map));

$m51_coords = pdl(0,0)->apply(t_fits($m51));
$m51map_coords = pdl(0,0)->apply(t_fits($m51map));
ok(all(approx($m51_coords, $m51map_coords,1e-8)));


########################################
########################################
###
### Give map a workout...

##############################
# Basic testing of resampling methods

$a = rvals(7,7) == 0;

$b = $a->match($a,{method=>'s'});
ok(all($a==$b),1,"self-match with 's' method is a no-op");

$b = $a->match($a,{method=>'l'});
ok(all(approx($a,$b)),1,"self-match with 'l' method is an approximate no-op");

$b = $a->match($a,{method=>'h'});
ok(all(approx($a,$b)),1,"self-match wtih hanning method is an approximate no-op");

$b = $a->match($a,{method=>'h',blur=>2});
$b0 = zeroes($a);
$b0->slice([2,4],[2,4]) .= pdl([[0.0625,0.125,0.0625],[0.125,0.25,0.125],[0.0625,0.125,0.0625]]);
ok(all(approx($b,$b0)),1,"self-match with hanning method and blur of 2 blurs right");

$b = $a->match($a,{method=>'g'});
$b0 = zeroes($a)-9;
$bc = pdl([-9,-3.3658615,-2.7638017],[-3.3658615,-1.5608028,-0.95874296],[-2.7638017,-0.95874296,-0.35668313]);
#$bc = pdl([-9,-9,-2.762678-4.4e-8],[-9,-1.5593078,-0.95724797],[-2.762678-4.4e-8,-0.95724797,-0.35518814]);

$b0->slice([1,3],[1,3]) .= $bc;
$b0->slice([5,3],[1,3]) .= $bc;
$b0->slice([1,5],[5,4]) .= $b0->slice([1,5],[1,2]);
ok(all(approx($b->clip(1e-9)->log10,$b0,1e-7)),1,"self-match with Gaussian method gives understood blur");

$t = t_linear(pre=>[0.5,1]);
$b = $a->map($t,{method=>'s',pix=>1});
$wndb = $b->whichND;
ok($wndb->nelem==2 and all($wndb==pdl([[3,4]])) and approx($b->slice(3,4),1),1,'offset with sample is a simple offset');

$b = $a->map($t,{method=>'l',pix=>1});
$wndb = $b->whichND;
ok($wndb->nelem==4 and all($wndb==pdl([[3,4],[4,4]])) and all(approx($b->slice([3,4],4),0.5)),1,'offset with linear interpolation does the right thing');

$b = $a->map($t,{method=>'h',pix=>1});
$wndb = $b->whichND;
ok($wndb->nelem==4 and all($wndb==pdl([[3,4],[4,4]])) and all(approx($b->slice([3,4],4),0.5)),1,'offset with hanning interpolation does the right thing');

##############################
# Test boundary conditions
$a = sequence(5,5);
$b = $a->match([10,10],{pix=>1,method=>'s'});
ok( all($b->slice([0,4],[0,4])==$a) && all($b->slice([5,9])==0) && all($b->slice('x',[5,9])==0), 1, "truncation boundary condition works");

$b = $a->match([10,10],{pix=>1,method=>'h'});
ok( all($b->slice([0,4],[0,4])==$a) && all($b->slice([5,9])==0) && all($b->slice('x',[5,9])==0), 1, "truncation boundary condition works for jacobian methods");

$b = $a->match([10,10],{pix=>1,method=>'s',bound=>'mp'});
ok( all($b->slice([0,4],[0,4])==$a) && all($b->slice([9,5])==$b->slice([0,4])) && all($b->slice('x',[5,9])==$b->slice('x',[0,4])), 1, "periodic and mirror boundary conditions work");

$b = $a->match([10,10],{pix=>1,method=>'h',bound=>'mp'});
ok( all($b->slice([0,4],[0,4])==$a) && all($b->slice([9,5])==$b->slice([0,4])) && all($b->slice('x',[5,9])==$b->slice('x',[0,4])), 1, "periodic and mirror boundary conditions work for jacobian methods");

