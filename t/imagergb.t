sub ok {
	my $no = shift ;
	my $result = shift ;
	print "not " unless $result ;
	print "ok $no\n" ;
}

sub tapprox {
	my($a,$b,$mdiff) = @_;
	$mdiff = 0.01 unless defined($mdiff);
	$c = abs($a-$b);
	$d = max($c);
	$d < $mdiff;
}

sub vars_ipv {
  PDL::Dbg::vars if $PDL::debug;
}

sub p {
  print @_ if $PDL::debug;
}

use PDL::LiteF;
use PDL::ImageRGB;
use PDL::Dbg;

$PDL::debug = 0;

print "1..6\n";

$im = float [1,2,3,4,5];

vars_ipv;

$out = bytescl($im,100);
ok(1,tapprox($im,bytescl($im,100)));
ok(2,$out->get_datatype == $PDL::Types::PDL_B);
$out = bytescl($im,-100);
ok(3,tapprox(pdl([0,25,50,75,100]),$out));

p "$out\n";

$rgb = double [[1,1,1],[1,0.5,0.7],[0.1,0.2,0.1]];
$out = rgbtogr($rgb);
ok(4,tapprox($out,pdl([1,0.67,0.16])));
ok(5,$out->get_datatype == $PDL::Types::PDL_D);

vars_ipv;
p $out;

$im = byte [[1,2,3],[0,3,0]];
$lut = byte [[0,0,0],
	     [10,1,10],
	     [2,20,20],
	     [30,30,3]
	    ];
# do the interlacing the lengthy way
$interl = zeroes(byte,3,$im->dims);
for $i (0..($im->dims)[0]-1) {
  for $j (0..($im->dims)[1]-1) {
	$pos = $im->at($i,$j);
	($tmp = $interl->slice(":,($i),($j)")) .= $lut->slice(":,($pos)");
  }
}
$tmp = 0; # -w shut up!

$out = interlrgb($im,$lut);
vars_ipv;
p $out;
ok(6,tapprox($out,$interl));
