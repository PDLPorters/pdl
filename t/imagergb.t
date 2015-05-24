use Test::More tests => 6;

use strict;
use warnings;

sub vars_ipv {
  PDL::Dbg::vars() if $PDL::debug;
}

sub p {
  print @_ if $PDL::debug;
}

use PDL::LiteF;
use PDL::ImageRGB;
use PDL::Dbg;

$PDL::debug = 0;


vars_ipv;

{
	my $im = float [1,2,3,4,5];
	my $out = bytescl($im,100);
	ok(all approx($im,$out));
	ok($out->get_datatype == $PDL::Types::PDL_B);
}

{
	my $im = float [1,2,3,4,5];
	my $out = bytescl($im,-100);
	ok(all approx(pdl([0,25,50,75,100]),$out));

	p "$out\n";
}

{
	my $rgb = double [[1,1,1],[1,0.5,0.7],[0.1,0.2,0.1]];
	my $out = rgbtogr($rgb);
	ok(all approx($out,pdl([1,0.67,0.16]), 0.01));
	ok($out->get_datatype == $PDL::Types::PDL_D);

	vars_ipv;
	p $out;
}

{
	my $im = byte [[1,2,3],[0,3,0]];
	my $lut = byte [[0,0,0],
		[10,1,10],
		[2,20,20],
		[30,30,3]
	];
	# do the interlacing the lengthy way
	my $interl = zeroes(byte,3,$im->dims);
	for my $i (0..($im->dims)[0]-1) {
		for my $j (0..($im->dims)[1]-1) {
			my $pos = $im->at($i,$j);
			(my $tmp = $interl->slice(":,($i),($j)")) .= $lut->slice(":,($pos)");
		}
	}

	my $out = interlrgb($im,$lut);
	vars_ipv;
	p $out;
	ok(all approx($out,$interl));
}
