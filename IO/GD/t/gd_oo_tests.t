# Tests for the OO interface of PDL::IO::GD.
# Judd Taylor, Orbital Sytstems, Ltd.
# 07 Apr 2006

use strict;
use warnings;
use PDL;
use Test::More;
use File::Temp qw(tempdir);
use PDL::IO::GD;

sub tapprox {
    my $x = shift;
    my $y = shift;
    my $d = abs($x - $y);
    #ok( all($d < 1.0e-5) );
    return all($d < 1.0e-5);
}

my $tempdir = tempdir( CLEANUP => 1 );
my $lutfile = "$tempdir/default.rcols";
my $testfile1 = "$tempdir/test.png";
my $testfile2 = "$tempdir/test2.png";
my $testfile3 = "$tempdir/test3.png";

# Write out the lutfile below, so we don't have to include it in the distro:
write_lut($lutfile);

#diag "Test writing byte (8bit) PNG image...\n";
my $pdl = sequence(byte, 30, 30);

my $lut = load_lut( $lutfile );
ok( ($lut->dim(0) == 3 && $lut->dim(1) == 256), 'Load a lut from an ASCII file' );

write_png( $pdl, $lut, $testfile1 );

write_true_png(sequence(100, 100, 3), $testfile3);

my $gd = PDL::IO::GD->new( { filename => $testfile1 } );
#diag "Object created!\n";
ok( defined( $gd ), 'Object created' );

my $x = $gd->gdImageSX();
ok( $x, 'query X dim' );
my $y = $gd->gdImageSY();
ok( $y, 'query Y dim' );

my $pdl2 = $gd->to_pdl;
ok( tapprox( $pdl, $pdl2 ), 'image matches original pdl' );

my $pdl3 = $gd->to_rgb->slice(',-1:0');
ok( tapprox( $pdl3, $pdl ), 'rgb image matches original pdl' )
  or diag 'orig(0:3,0:3)=', $pdl->slice('0:3,0:3'),
    'new(0:3,0:3)=', $pdl3->slice('0:3,0:3');

undef $gd;

my $im = PDL::IO::GD->new( { x => 300, y => 300 } );
ok( defined( $im ), 'create new image from scratch' );

$im->apply_lut( $lut );

# Resolve some colors:
my $black = $im->ColorResolve( 0, 0, 0 );
ok( defined( $black ), 'resolve color black' );
my $red = $im->ColorResolve( 255, 0, 0 );
ok( defined( $red ), 'resolve color red' );
my $green = $im->ColorResolve( 0, 255, 0 );
ok( defined( $green ), 'resolve color green' );
my $blue = $im->ColorResolve( 0, 0, 255 );
ok( defined( $blue ), 'resolve color blue' );

# Draw a rectangle:
$im->Rectangle( 5, 5, 295, 295, $red );
ok( 1, 'draw a rectangle' );

# Add some text:
$im->String( gdFontGetLarge(), 10, 10, "Test Large Font!", $green );
ok( 1, 'add some text' );

# Generate a color bar:
my $x1 = zeroes( long, 256 ) + 50;
my $y1 = sequence( long, 256 ) + 30;
my $color = sequence(long, 256);
$im->Lines( $x1, $y1, $x1 + 100, $y1, $color );
ok( 1, 'generate a color bar' );

# Write the output file:
$im->write_Png( $testfile2 );
ok( 1, 'write the output file' );
undef $im;

my $pic = sequence(100, 100);
$im = PDL::IO::GD->new({ pdl => $pic });
ok( defined( $im ), 'create from 2d PDL without a LUT' );
undef $im;

$im = PDL::IO::GD->new({ pdl => $pic, lut => $lut });
ok( defined( $im ), 'create from 2d PDL and a LUT' );
undef $im;

my $pic3d = $pic->dummy(2,3);
$im = PDL::IO::GD->new({ pdl => $pic3d });
ok( defined( $im ), 'create from a RGB PDL' );
undef $im;

$im = PDL::IO::GD->new({ x => 100, y => 100, true_color => 1 });
ok( defined( $im ), 'create an RGB from scratch' );
undef $im;

# Create from a 2d PNG data glob:
my $rc = open( TF1, $testfile1 );
ok( $rc, 'opened test file and handle' );
binmode( TF1 );
$/ = undef;
my $blob = <TF1>;
close( TF1 );
$im = PDL::IO::GD->new({ data => $blob });
ok( defined( $im ), 'create from a 2d PNG data glob' );
undef $im;

# Create from a 2d PNG data glob, with the type given:
$im = PDL::IO::GD->new({ data => $blob, type => 'png' });
ok( defined( $im ), 'create from glob with type given' );
undef $im;

# Create from a 3d PNG data glob:
$rc = open( TF3, $testfile3 );
ok( $rc , 'testfile3 successfully opened');
binmode( TF3 );
$/ = undef;
my $blob3d = <TF3>;
close( TF3 );
$im = PDL::IO::GD->new({ data => $blob3d });
ok( defined( $im ), 'create from a 3d PNG data glob' );
# Get a PNG data glob from a created 
my $png_blob = $im->get_Png_data();
ok( $blob3d eq $png_blob, 'get a PNG data glob' );
undef $im;

# Try a nicer way to make an object. Just pass in a filename:
my $gd_new_just_filename = PDL::IO::GD->new( $testfile1 );
ok( defined( $gd_new_just_filename ), 'initialize an object from JUST the filename' );

# Try another nicer way to make an object: Pass in an inline hash:
my $gd_new_inline_hash = PDL::IO::GD->new( filename => $testfile1 );
ok( defined( $gd_new_inline_hash ), 'initialize an object from an inline hash' );

# Make sure bogus inline hashes generate complaints. First, give an odd
# number of args
my $gd_new_inline_hash_broken1;
eval { $gd_new_inline_hash_broken1 = PDL::IO::GD->new( filename => $testfile1, 34 ) };
ok( $@ && !defined( $gd_new_inline_hash_broken1 ), 'incorrectly initialize an object from an inline hash: odd Nargs' );
# TEST 32:
# Make sure bogus inline hashes generate complaints. Give a non-string key
my $gd_new_inline_hash_broken2;
eval { $gd_new_inline_hash_broken2 = PDL::IO::GD->new( filename => $testfile1, [34] => 12 ) };
ok( $@ && !defined( $gd_new_inline_hash_broken2 ), 'incorrectly initialize an object from an inline hash: non-string key' );

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
