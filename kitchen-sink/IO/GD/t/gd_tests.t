#!/usr/bin/perl

#
# t/gd_tests.t - tests functions in the PDL::IO::GD module
#
# Judd Taylor, USF IMaRS
# 13 March 2003
#

use strict;
use PDL;
use PDL::Config;
use Test::More;
use File::Temp qw(tempdir);

BEGIN
{
    use PDL::Config;
    if ( $PDL::Config{WITH_GD} ) 
    {
        eval( " use PDL::IO::GD; " );
        if( $@ )
        {
            plan skip_all => "PDL::IO::GD requires the gd image library.";
        }  
        else
        {
            plan tests => 13;
        }
    }
    else
    {
        plan skip_all => "PDL::IO::GD not compiled.";
    }
}

sub tapprox
{
    my $a = shift;
    my $b = shift;
    my $d = abs($a - $b);
    #ok( all($d < 1.0e-5) );
    return all($d < 1.0e-5);
}

use ExtUtils::testlib;

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

# Start the tests:
#

# TEST 1:
# Load the lutfile from the ascii file we just created:
my $lut = load_lut( $lutfile );
print "Dims of loaded lut: " . join(", ", $lut->dims()) . "\n";
ok( ($lut->dim(0) == 3 && $lut->dim(1) == 256) );

# TEST 2:
print "Test writing byte (8bit) PNG image...\n";
my $pdl = sequence(byte, 30, 30);
write_png( $pdl, $lut, $testfile1 );
ok( 1 );

# TEST 3:
print "Testing writing true color (32 bit) PNG image...\n";
my $tc_pdl = sequence(byte, 100, 100, 3);
write_true_png( $tc_pdl, $testfile2 );
ok( 1 );

# TEST 4:
print "Test reading byte (8bit) PNG image...\n";
my $image = read_png($testfile1);
ok( tapprox( $pdl, $image ) );
$image = null;

# TEST 5:
print "Test reading true color PNG image...\n";
$image = read_true_png( $testfile2 );
ok( tapprox( $image, $tc_pdl ) );

# TEST 6:
print "Test reading byte (8bit) PNG Color Table...\n";
my $lut2 = read_png_lut( $testfile1 );
ok( tapprox( $lut, $lut2 ) );

# TESTS 7-9:
# Test the compression level stuff:
print "Test writing byte (8bit) PNG image with various compression levels...\n";
$pdl = sequence(byte, 30, 30);
write_png_ex($pdl, $lut, $testfile3, 0);
ok( 1 );
write_png_ex($pdl, $lut, $testfile3, 9);
ok( 1 );
write_png_best($pdl, $lut, $testfile3);
ok( 1 );

# TESTS 10-12:
print "Testing writing true color (32 bit) PNG image with various compression levels...\n";
$pdl = sequence(100, 100, 3);
write_true_png_ex($pdl, $testfile4, 0);
ok( 1 );
write_true_png_ex($pdl, $testfile3, 9);
ok( 1 );
write_true_png_best($pdl, $testfile3 );
ok( 1 );

# TEST 13:
print "Testing recompressiong PNG image recompress_png_best()...\n";
recompress_png_best( $testfile3 );
ok( tapprox( read_png( $testfile4 ), read_png( $testfile3 ) ) );

# Remove the testfiles: 
#
for ( $lutfile, $testfile1, $testfile2, $testfile3, $testfile4 )
    { unlink( $_ ); }

exit(0);

sub write_lut
{
    my $filename = shift;
    open( LUT, ">$filename" )
        or die "Can't write $filename: $!\n";
    print LUT <<'ENDLUT';
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
    close( LUT );
} # End of write_lut()...

