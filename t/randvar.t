
use strict;
use Test;

BEGIN { plan tests => 7; }

use PDL;
#use PDL::IO::Pic;
#use PDL::ImageRGB;
#use PDL::Dbg;
#$PDL::debug = 0;

# declare variables
my ( $a, $a1, $a_vec, $b, $c, $d );

## 1: Make sure the library loads
eval 'use PDL::RandVar;';
if($@) {print $@,"\n";}
ok( $@, "" );

### 2: Make a new random variable
eval '$a = new PDL::RandVar({range=>pdl(0,10)});';
ok( $@, "" );

### 3: Make sure its mean is correct
$b = $a->sample(500);
$c = $b->sum / 500;
$d = ($c<5.3 && $c>4.7);
if(!$d) {
	$b = $a->sample(500);
	$c = $b->sum / 500;
	$d = ($c<5.3 && $c>4.7);
}
ok($d);

### 4: Try Sobol variables
eval 'use PDL::RandVar::Sobol';
ok($@,"");

### 5: Make sure there's no short cyclic behavior
## (most common problem with subrands)
$a = new PDL::RandVar::Sobol;
$a1 = $a->sample();
$b = $a->sample(1000);
ok( any($b==$a1), 0 );

### 6: Make sure histograms load
eval 'use PDL::RandVar::Histogram';
ok($@,"");

### 7: Test histogram distribution
$a_vec = pdl(1,5);
$a = new PDL::RandVar::Histogram($a_vec);
for(1..2){
	$b = $a->sample(1200)->floor;
	$c = ($b==1)->sum / 1000;
	$d = ($c>0.98 && $c<1.02);
	last if($d) ;
}
ok($d);

# End of tests
#
