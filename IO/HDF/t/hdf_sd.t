# Tests the SD interface to the HDF library.
#
# Judd Taylor, Orbital Systems, Ltd.
# 29 March 2006
#
use strict;
use PDL;
use Test::More;
use File::Temp qw(tempdir);
use PDL::IO::HDF::SD;

sub tapprox {
    my $x = shift;
    my $y = shift;
    my $d = abs($x - $y);
    #ok( all($d < 1.0e-5) );
    return all($d < 1.0e-5);
}

my $tmpdir = tempdir( CLEANUP => 1 );
my $testfile = "$tmpdir/sdtest.hdf";

my $SDobj = PDL::IO::HDF::SD->new( "-$testfile" );
        
my $data = sequence(short, 500, 5);
my $square_data = sequence(short, 50, 50);

ok( $SDobj->SDput("myData", $data , ['dim1','dim2']), 'SDput()' );

ok( $SDobj->SDsetfillvalue("myData", 0), 'SDsetfillvalue()' );

ok( $SDobj->SDsetrange("myData", [0, 2000]), 'SDsetrange()' );

ok( $SDobj->SDsetcal("myData"), 'SDsetcal()' );

ok( $SDobj->SDsettextattr('This is a global text test!!', "myGText" ), 'SDsettextattr() (global)' );

ok( $SDobj->SDsettextattr('This is a local text testl!!', "myLText", "myData" ), 'SDsettextattr() (local)' );

ok( $SDobj->SDsetvalueattr( PDL::short( 20 ), "myGValue"), 'SDSetvalueattr() (global)' );

ok( $SDobj->SDsetvalueattr( PDL::long( [20, 15, 36] ), "myLValues", "myData" ), 'SDSetvalueattr() (local)' );

ok( $SDobj->SDput("mySquareData", $square_data , ['square_dim','square_dim']), 'SDput()' );

$SDobj->close;

ok( PDL::IO::HDF::SD::Hishdf( $testfile ), 'Hishdf()' );
    
#Open an HDF file in read only mode
my $SDobj2 = PDL::IO::HDF::SD->new( $testfile );

my @dataset_list = $SDobj2->SDgetvariablenames();
ok( $#dataset_list+1, 'SDgetvariablenames()' );

my @globattr_list = $SDobj2->SDgetattributenames();
ok( $#globattr_list+1, 'SDgetattributenames() (global)' );

my @locattr_list = $SDobj2->SDgetattributenames( "myData" );
ok( $#locattr_list+1, 'SDgetattributenames() (local)' );

my $value = $SDobj2->SDgetattribute( "myLText", "myData" );
ok( defined($value), 'SDgetattribute() (local)' );

$data = $SDobj2->SDget("myData");
ok( $data->nelem() > 0, 'SDget()' );

my @dim = $SDobj2->SDgetdimnames("myData");
ok( ($dim[0] eq "dim1") && ($dim[1] eq "dim2") , 'SDgetdimnames()' );

my @dim_square = $SDobj2->SDgetdimsize("myData");
ok( ($dim_square[0] == 5) && ($dim_square[1] == 500), 'SDgetdimsize()' );

@dim_square = $SDobj2->SDgetdimnames("mySquareData");
ok( ($dim_square[0] eq "square_dim") && ($dim_square[1] eq "square_dim"), 'SDgetdimnames()' );

@dim_square = $SDobj2->SDgetdimsize("mySquareData");
ok( ($dim_square[0] == 50) && ($dim_square[1] == 50), 'SDgetdimsize()' );

my $square_data_get = $SDobj2->SDget("mySquareData");
ok( $square_data_get->nelem() > 0, 'SDget()' );

my $res = $SDobj2->SDgetscalefactor("myData");
ok( defined($res), 'SDgetscalefactor()' );

#The fill value corresponding to the BAD value in pdl
$res = $SDobj2->SDgetfillvalue("myData");
ok( defined($res), 'SDgetfillvalue()' );

my @range = $SDobj2->SDgetrange("myData");
ok( $#range+1, 'SDgetrange()' );
 
$SDobj2->close;

undef($data);
my $HDFobj = PDL::IO::HDF::SD->new("-$testfile");

$data = ones( short, 5000, 5);
ok( $HDFobj->SDput("myData", $data , ['dim1','dim2']), 'SDput()' );

$HDFobj->SDput("myData", $data , ['dim1','dim2']);
$data = $HDFobj->SDget("myData");
ok( $data->nelem(), 'SDget()' );

$HDFobj->close();

my $hdf = PDL::IO::HDF::SD->new( "-$testfile" );

ok( $hdf->Chunking(), 'Chunking()' );
$hdf->Chunking(0);
ok( !$hdf->Chunking(), 'Chunking(0)' );

my $dataset = sequence( byte, 10, 10 );
$res = $hdf->SDput( "NO_CHUNK", $dataset );
ok( $res, 'SDput() (unchunked)' );

$hdf->close();
undef($hdf);

$hdf = PDL::IO::HDF::SD->new( $testfile );

my $dataset_test = $hdf->SDget( "NO_CHUNK" );
my $good = ($dataset_test->nelem() > 0) ? 1 : 0;
ok( $good, 'SDget() (unchunked)' );
my $do_skip = $good ? '' : 'Skip if failed previous test!';
SKIP: {
    skip( "Previous test failed!", 1 ) if $do_skip;
    ok( tapprox( $dataset, $dataset_test ), 'comparing datasets written out and read in (unchunked)' );
}

$hdf->close();
undef($hdf);
unlink( $testfile );

# Reopen to write out the chunked portion:
$hdf = PDL::IO::HDF::SD->new( "-$testfile" );

my $dataset2d = sequence( long, 200, 200 );

$res = $hdf->SDput( "CHUNK_2D", $dataset2d );
ok( $res, 'SDput() (chunked, 2D)' );

my $dataset3d = sequence( long, 200, 200, 10 );
$res = $hdf->SDput( "CHUNK_3D", $dataset3d );
ok( $res, 'SDput() (chunked, 3D)');

$hdf->close();
undef($hdf);

# Verify the datasets we just wrote:
$hdf = PDL::IO::HDF::SD->new( $testfile );

my $dataset2d_test = $hdf->SDget( "CHUNK_2D" );
$good = $dataset2d_test->nelem() > 0;
ok( $good, 'SDget() (chunked, 2D)' );
$do_skip = $good ? '' : 'Skip if failed previous test!';
SKIP: {
    skip( "Previous test failed!", 1 ) if $do_skip;
    ok( tapprox( $dataset2d, $dataset2d_test ), 'comparing datasets written out and read in (chunked, 2D)' );
}

my $dataset3d_test = $hdf->SDget( "CHUNK_3D" );
$good = $dataset3d_test->nelem() > 0;
ok( $good, 'SDget() (chunked, 3D)' );
$do_skip = $good ? '' : 'Skip if failed previous test!';
SKIP: {
    skip( "Previous test failed!", 1 ) if $do_skip;
    ok( tapprox( $dataset3d, $dataset3d_test ), 'comparing datasets written out and read in (chunked, 3D)' );
}

$hdf->close();
undef($hdf);

done_testing;
