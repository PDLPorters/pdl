
BEGIN {
 print "1..7\n";
}

use PDL;
use PDL::IO::Pic;
use PDL::ImageRGB;
use PDL::Dbg;
$PDL::debug = 0;

sub ok($;$$){
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}


## 1: Make sure the library loads
my($n);
print STDERR ++$n,"...";

eval 'use PDL::RandVar;';
if($@) {print $@,"\n";}
ok(1,!$@);

### 2: Make a new random variable
print STDERR  ++$n,"...";
eval '$a = new PDL::RandVar({range=>pdl(0,10)});';
ok(2,!$@);

### 3: Make sure its mean is correct
print STDERR  ++$n,"...";
$b = $a->sample(500);
$c = $b->sum / 500;
$d = ($c<5.3 && $c>4.7);
if(!$d) {
	$b = $a->sample(500);
	$c = $b->sum / 500;
	$d = ($c<5.3 && $c>4.7);
}
ok(3,$d);

### 4: Try Sobol variables
print STDERR  ++$n,"...";
eval 'use PDL::RandVar::Sobol';
ok(4,!$@);

### 5: Make sure there's no short cyclic behavior
## (most common problem with subrands)
print STDERR  ++$n,"...";
$a = new PDL::RandVar::Sobol;
$a1 = $a->sample();
$b = $a->sample(1000);
ok(5,!(($b==$a1)->sum));

### 6: Make sure histograms load
print STDERR  ++$n,"...";
eval 'use PDL::RandVar::Histogram';
ok(6,!$@);

### 7: Test histogram distribution
print STDERR  ++$n,"...";
$a_vec = pdl(1,5);
$a = new PDL::RandVar::Histogram($a_vec);
for(1..2){
	$b = $a->sample(1200)->floor;
	$c = ($b==1)->sum / 1000;
	$d = ($c>0.98 && $c<1.02);
	last if($d) ;
}
ok(7,$d);





