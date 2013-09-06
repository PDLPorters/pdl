use PDL::LiteF;
use Test;

BEGIN {
  plan tests => 10;
}

use PDL::Transform;

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
print "apply\n";
$a->inplace->apply($t);
print "add q\n";
$a += $q;
ok(1);  # still here!

##############################
# bad value handling...

if($PDL::Bad::Status) {
    $a = sequence(5,5);
    $t1 = t_linear(pre=>[1.5,2]);
    $t2 = t_linear(pre=>[1,2]);

    $a->badflag(1);

    eval q{$b = $a->map($t1,{pix=>1,method=>'l'});};
    ok(!$@);

    ok($b->slice("0:1")->isbad->all  and  $b->slice(":,0:1")->isbad->all  and $b->isbad->sum==16, "Bad values happen");

    eval q{$b = $a->map($t1,{pix=>1,method=>'h'});};
    ok($b->slice("0")->isbad->all  and  $b->slice(":,0:1")->isbad->all and $b->isbad->sum==13, "Bad values happen with 'h' method"); 
    

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
