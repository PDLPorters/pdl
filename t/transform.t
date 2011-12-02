no warnings qw(misc);
use PDL::LiteF;
use Test;

BEGIN {
  plan tests => 4;
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


