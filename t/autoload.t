
# Test PDL::AutoLoader

use PDL::LiteF;
use PDL::AutoLoader;

print "1..1\n";

@PDLLIB = ("t/");

$x = long(2 + ones(2,2));

$y = func($x);

if (sum($y) == 4*29) {
   print "ok 1\n";
}
else{
   print "not ok 1\n";
}

