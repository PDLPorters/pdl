use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use PDL::Compression;
use PDL::IO::FITS;

my $m51 = rfits('../../m51.fits');

my ($y, $xsize, undef, $len) = $m51->rice_compress;
is $len->max->sclr, 373, 'right maximum length';
my $m51_2 = eval { $y->rice_expand($len, $xsize) };
if (is $@, '', 'no error') {
ok all(approx($m51, $m51_2)), 'decompress same as original';
}

my $expected = pdl(byte, '[[126 122 122 128 128 124 124 128 128 128 127 126 126 127 127 128 124 124 123 123 122 122 121 121 120 120 119 119 118 118 117 117 118 118 117 116 115 114 113 113 116 115 115 114 114 113 112 112 111 111 110 110 110 110 110 110 109 109 110 110 110 111 111 111]]');
my $compressed_correct = pdl(byte, '[[126 48 24 0 96 48 14 179 32 54 219 109 147 85 96 91 91 126 206 112]]');
my $got = eval { $compressed_correct->rice_expand($compressed_correct->dim(0), 64) };
is $@, '', 'no error';
ok all(approx($got, $expected)), 'decompress correct version gives right answer' or diag "got=${got}expected=$expected";
($y, $xsize, undef, $len) = $expected->rice_compress(32);
$got = eval { $y->rice_expand($len, $xsize) };
is $@, '', 'no error';
ok all(approx($got, $expected)), 'decompress same as original (2)' or diag "got=${got}expected=$expected";

done_testing;
