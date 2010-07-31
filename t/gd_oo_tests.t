#!/usr/bin/perl

#
# Tests for the OO interface: PDL::IO::GDImage;
#
# Judd Taylor, USF IMaRS
# 07 Apr 2006
#

use strict;
use PDL;
use Test::More;

BEGIN
{
    eval( " use PDL::IO::GD; " );
    if( $@ )
    {
        plan skip_all => "Skipped: PDL::IO::GD requires the gd image library.";
    }  
    else
    {
        plan tests => 27;
    }
}

use ExtUtils::testlib;

use PDL::IO::GD;
use PDL::Config;


sub tapprox
{
    my $a = shift;
    my $b = shift;
    my $d = abs($a - $b);
    #ok( all($d < 1.0e-5) );
    return all($d < 1.0e-5);
}

# Test files:
#
my $tempdir = $PDL::Config{TEMPDIR} || "/tmp";

my $lutfile = "$tempdir/default.rcols";
my $testfile1 = "$tempdir/test.png";
my $testfile2 = "$tempdir/test2.png";
my $testfile3 = "$tempdir/test3.png";

# Write out the lutfile below, so we don't have to include it in the distro:
write_lut($lutfile);

# Start the tests:
#

print "Test writing byte (8bit) PNG image...\n";
my $pdl = sequence(byte, 30, 30);

# TEST 1:
# Load a lut from an ASCII file:
#print "\$pdl:\n$pdl\n";
my $lut = load_lut( $lutfile );
#print "\$lut info(): " . $lut->info() . "\n";
#print "\$lut:\n$lut\n";
ok( ($lut->dim(0) == 3 && $lut->dim(1) == 256) );

# TEST 2:
# write a PNG with the old interface:
write_png( $pdl, $lut, $testfile1 );
ok(1);

# TEST 3:
# write a truecolor PNG with the old interface:
print "Testing writing true color (32 bit) PNG image...\n";
write_true_png(sequence(100, 100, 3), $testfile3);
ok(1);


#
# Open the file:
#
# TEST 4:
# Create a new object:
my $gd = PDL::IO::GD->new( { filename => $testfile1 } );
print "Object created!\n";
ok( defined( $gd ) );

# TEST 5 & 6:
# Query the dims:
my $x = $gd->gdImageSX();
ok( $x );
my $y = $gd->gdImageSY();
ok( $y );
print "\$x = $x\t\$y = $y\n";

# TEST 7:
# Read it into a PDL, and make sure it matches:
my $pdl2 = $gd->to_pdl();
ok( tapprox( $pdl, $pdl2 ) );

# TEST 8:
# Kill it:
$gd->DESTROY();
print "Object destroyed!\n";
ok( 1 );

#
# Create a new object:
# 
# TEST 9:
# Create a new image from scratch:
my $im = PDL::IO::GD->new( { x => 300, y => 300 } );
ok( defined( $im ) );

#
# Allocate some colors:
#
# TEST 10:
$im->apply_lut( $lut );
ok( 1 );

# TESTS 11-14:
# Resolve some colors:
my $black = $im->ColorResolve( 0, 0, 0 );
ok( defined( $black ) );
my $red = $im->ColorResolve( 255, 0, 0 );
ok( defined( $red ) );
my $green = $im->ColorResolve( 0, 255, 0 );
ok( defined( $green ) );
my $blue = $im->ColorResolve( 0, 0, 255 );
ok( defined( $blue ) );

# TEST 15:
# Draw a rectangle:
$im->Rectangle( 5, 5, 295, 295, $red );
ok( 1 );

# TEST 16:
# Add some text:
$im->String( gdFontGetLarge(), 10, 10, "Test Large Font!", $green );
ok( 1 );

# TEST 17:
# Generate a color bar:
my $x1 = zeroes( long, 256 ) + 50;
my $y1 = sequence( long, 256 ) + 30;
my $color = sequence(long, 256);
$im->Lines( $x1, $y1, $x1 + 100, $y1, $color );
ok( 1 );

# TEST 18:
# Write the output file:
$im->write_Png( $testfile2 );
ok( 1 );
$im->DESTROY(); $im = undef;

#
# New tests on object creation:
#

# TEST 19:
# Create from a 2d PDL without a LUT:
my $pic = sequence(100, 100);
$im = PDL::IO::GD->new({ pdl => $pic });
ok( defined( $im ) );
$im->DESTROY(); $im = undef;

# TEST 20:
# Create from a 2d PDL and a LUT:
$im = PDL::IO::GD->new({ pdl => $pic, lut => $lut });
ok( defined( $im ) );
$im->DESTROY(); $im = undef;

# TEST 21:
# Create from a RGB PDL:
my $pic3d = $pic->dummy(2,3);
$im = PDL::IO::GD->new({ pdl => $pic3d });
ok( defined( $im ) );
$im->DESTROY(); $im = undef;

# TEST 22:
# Create an RGB from scratch:
$im = PDL::IO::GD->new({ x => 100, y => 100, true_color => 1 });
ok( defined( $im ) );
$im->DESTROY(); $im = undef;

# TEST 23-24:
# Create from a 2d PNG data glob:
my $rc = open( TF1, $testfile1 );
ok( $rc );
binmode( TF1 );
$/ = undef;
my $blob = <TF1>;
close( TF1 );
$im = PDL::IO::GD->new({ data => $blob });
ok( defined( $im ) );
$im->DESTROY(); $im = undef;

# TEST 25:
# Create from a 2d PNG data glob, with the type given:
$im = PDL::IO::GD->new({ data => $blob, type => 'png' });
ok( defined( $im ) );
$im->DESTROY(); $im = undef;

# TEST 26-27:
# Create from a 3d PNG data glob:
$rc = open( TF3, $testfile3 );
ok( $rc );
binmode( TF3 );
$/ = undef;
my $blob3d = <TF3>;
close( TF3 );
$im = PDL::IO::GD->new({ data => $blob3d });
ok( defined( $im ) );
$im->DESTROY(); $im = undef;


# Remove our test files:
#
unlink( $lutfile );
unlink( $testfile1 );
unlink( $testfile2 );
unlink( $testfile3 );

exit (0);
# 
sub write_lut
{
    my $filename = shift;
    open( LUT, ">$filename" );
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

