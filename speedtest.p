
use PDL;

print "Using PDL version $PDL::VERSION\n";

$x = float zeroes (10,10); $y = float 1.3;

$start = time;

for ($i==0; $i<=500000; $i++) {

  $x*=$y;
}

print time-$start, " secs\n";
print sec($x,0,1,0,1);
