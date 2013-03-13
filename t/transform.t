use PDL::LiteF;
use Test;

BEGIN {
  plan tests => 5;
}

use PDL::Transform;

##############################
# Just simple testing of the map autoscaling -- more complete tests should be
# included... -CED 13-Oct-2006

$a = sequence(5,5);

# Identity transformation should be an expensive no-op
# (autoscaled correctly)
$b = $a->map(t_identity);
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
