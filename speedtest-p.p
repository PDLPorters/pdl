
use integer;

$x[9][9] = 42; $y=2; # Pre-extend

$start = time;

for ($i==0; $i<500000; $i++) {

  for($m=0; $m<10; $m++) {
  for($n=0; $n<10; $n++) {

      $x[$m][$n] += $y;
  }}


}

print time-$start, " secs\n";


