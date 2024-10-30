# t/gd_tests.t - tests functions in the PDL::IO::GD module
# Judd Taylor, USF IMaRS
# 13 March 2003

use strict;
use warnings;
use PDL;
use Test::More;
use Test::PDL;
use File::Temp qw(tempdir);
use PDL::IO::GD;

# Test Files:
my $tempdir = tempdir( CLEANUP=>1 );

my $lutfile = "$tempdir/default.rcols";
my $testfile1 = "$tempdir/test.png";
my $testfile2 = "$tempdir/test_true.png";
my $testfile3 = "$tempdir/test_comp.png";
my $testfile4 = "$tempdir/test_nocomp.png";

# Write out the lutfile below, so we don't have to include it in the distro:
write_lut($lutfile);

my $lut = load_lut( $lutfile );
ok( ($lut->dim(0) == 3 && $lut->dim(1) == 256) );

eval {write_png( sequence(16,16), sequence(255)->dummy(0,3), $testfile1 )};
like $@, qr/exceeded LUT size/, 'too-short LUT throws exception';
my $pdl = sequence(byte, 30, 30);
write_png( $pdl, $lut, $testfile1 );
is_pdl read_png($testfile1), $pdl->long;
eval {read_true_png($testfile1)};
like $@, qr/Tried to read a non-truecolour/, 'right error instead of segfault';
is_pdl read_png_lut( $testfile1 ), $lut;

my $tc_pdl = sequence(byte, 100, 100, 3);
write_true_png( $tc_pdl, $testfile2 );
is_pdl read_true_png( $testfile2 ), $tc_pdl;

$pdl = sequence(byte, 30, 30);
write_png_ex($pdl, $lut, $testfile3, 0);
write_png_ex($pdl, $lut, $testfile3, 9);
write_png_best($pdl, $lut, $testfile3);

$pdl = sequence(100, 100, 3);
write_true_png_ex($pdl, $testfile4, 0);
write_true_png_ex($pdl, $testfile3, 9);
write_true_png_best($pdl, $testfile3 );

recompress_png_best( $testfile3 );
is_pdl read_png( $testfile4 ), read_png( $testfile3 );

done_testing;

sub write_lut {
    my $filename = shift;
    open my $fh, ">", $filename or die "Can't write $filename: $!\n";
    print $fh <<'ENDLUT';
  2    0    4
  9    0    7
 22    0   19
 36    0   32
 50    0   48
 61    0   63
 69    0   77
 77    0   91
 82    0  104
 84    0  118
 88    0  132
 87    0  145
 84    0  159
 83    0  173
 77    0  186
 70    0  200
 60    0  214
 53    0  227
 40    0  241
 25    0  255
 12    0  255
  0    4  255
  0   21  255
  0   38  255
  0   55  255
  0   72  255
  0   89  255
  0  106  255
  0  119  255
  0  135  255
  0  152  255
  0  165  255
  0  187  255
  0  195  255
  0  203  255
  0  208  255
  0  220  255
  0  225  255
  0  233  255
  0  242  255
  0  250  255
  0  255  255
  0  255  242
  0  255  238
  0  255  225
  0  255  220
  0  255  212
  0  255  203
  0  255  195
  0  255  187
  0  255  174
  0  255  165
  0  255  152
  0  255  144
  0  255  131
  0  255  114
  0  255  102
  0  255   84
  0  255   67
  0  255   55
  0  255   38
  0  255   21
  0  255    8
  8  255    0
 25  255    0
 42  255    0
 55  255    0
 76  255    0
 97  255    0
119  255    0
140  255    0
161  255    0
182  255    0
203  255    0
225  255    0
246  255    0
255  242    0
255  242    0
255  242    0
255  242    0
255  238    0
255  238    0
255  238    0
255  233    0
255  233    0
255  233    0
255  233    0
255  229    0
255  229    0
255  229    0
255  221    0
255  221    0
255  221    0
255  221    0
255  216    0
255  216    0
255  216    0
255  212    0
255  212    0
255  212    0
255  208    0
255  208    0
255  199    0
255  199    0
255  199    0
255  195    0
255  195    0
255  191    0
255  191    0
255  191    0
255  187    0
255  187    0
255  178    0
255  178    0
255  178    0
255  174    0
255  174    0
255  170    0
255  170    0
255  165    0
255  165    0
255  165    0
255  161    0
255  161    0
255  153    0
255  153    0
255  153    0
255  148    0
255  148    0
255  144    0
255  144    0
255  144    0
255  140    0
255  140    0
255  131    0
255  131    0
255  127    0
255  127    0
255  127    0
255  123    0
255  123    0
255  123    0
255  119    0
255  119    0
255  110    0
255  110    0
255  110    0
255  106    0
255  106    0
255  106    0
255  102    0
255  102    0
255   97    0
255   97    0
255   97    0
255   89    0
255   89    0
255   85    0
255   85    0
255   85    0
255   80    0
255   80    0
255   76    0
255   76    0
255   72    0
255   72    0
255   72    0
255   63    0
255   63    0
255   59    0
255   59    0
255   55    0
255   55    0
255   51    0
255   51    0
255   42    0
255   42    0
255   38    0
255   38    0
255   34    0
255   34    0
255   29    0
255   29    0
255   21    0
255   21    0
255   17    0
255   17    0
255   17    0
255   17    0
255   17    0
255   17    0
255   17    0
255   17    0
255   12    0
255   12    0
255   12    0
255   12    0
255   12    0
255   12    0
255   12    0
255   12    0
255    8    0
255    8    0
255    8    0
255    8    0
255    8    0
255    8    0
255    8    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255    0    0
255   29    0
170  170  170
  0  255    0
255  250    0
  0    0    0
255  255  255
ENDLUT
} # End of write_lut()...
