use strict;
use warnings;
use Test::More;
use PDL::LiteF;
use PDL::Compression;
use PDL::IO::FITS;

my $m51 = rfits('../../m51.fits');

my ($y, $xsize, undef, $len) = $m51->rice_compress;
is $len->max->sclr, 373, 'right maximum length';
my $m51_2 = eval { $y->rice_expand($xsize) };
is $@, '', 'no error';
ok all(approx($m51, $m51_2)), 'decompress same as original';

done_testing;
