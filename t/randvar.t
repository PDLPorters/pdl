use PDL;
use PDL::IO::Pic;
use PDL::ImageRGB;
use PDL::Dbg;

$PDL::debug = 0;

sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}


## 1: Make sure the library loads
eval 'use PDL::Lib::RandVar;'
ok(1,!$@);

### 2: Make a new random variable
eval '$a = new PDL::Lib::RandVar({range=>pdl(0,10)});';
ok(2,!$@);

### 3: Make sure its mean is correct
$b = $a->sample(1000);
$c = $b->sum / 100;
$d = ($c<5.2 && $c>4.8);
if(!$d) {
	$b = $a->sample(1000);
	$c = $b->sum / 100;
	$d = ($c<5.2 && $c>4.8);
}
ok(3,$d);

### 4: Try Sobol variables
eval 'use PDL::Lib::RandVar::Sobol';
ok(4,!$@);

### 5: Make sure there's no short cyclic behavior
## (most common problem with subrands)
$a = new PDL::Lib::RandVar::Sobol;
$a1 = $a->sample();
$b = $a->sample(1000);
ok(5,!(($b==$a1)->sum));

### 6: Make sure histograms load
eval 'use PDL::Lib::RandVar::Histogram';
ok(6,!$@);

### 7: Test histogram distribution
$a_vec = pdl(1,5);
$a = new PDL::Lib::RandVar::Histogram($a_vec);
for(1..2){
	$b = $a->sample(1200)->floor;
	$c = ($b==1)->sum / 1000;
	$d = ($c>0.98 && $c<1.02);
	if($d) break;
}
ok(7,$d);





