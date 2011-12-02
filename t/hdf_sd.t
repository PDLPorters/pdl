#!/usr/bin/perl -w
no warnings qw(misc);
#
# t/hdf_sd.t
#
# Tests the SD interface to the HDF library.
#
# Judd Taylor, Orbital Systems, Ltd.
# 29 March 2006
#
use strict;
use PDL;
use Test::More;

BEGIN
{
    use PDL::Config;
    if ( $PDL::Config{WITH_HDF} ) 
    {
        eval( " use PDL::IO::HDF; " );
        if( $@ )
        {
            plan skip_all => "PDL::IO::HDF module compiled, but not available.";
        }  
        else
        {
            plan tests => 36;
        }
    }
    else
    {
        plan skip_all => "PDL::IO::HDF module not compiled.";
    }
}

use ExtUtils::testlib;

sub tapprox
{
    my $a = shift;
    my $b = shift;
    my $d = abs($a - $b);
    #ok( all($d < 1.0e-5) );
    return all($d < 1.0e-5);
}

use PDL::Config;
my $tmpdir = $PDL::Config{TEMPDIR};

my $testfile = "$tmpdir/sdtest.hdf";

use PDL::IO::HDF::SD;

### Creating and writing to a HDF file
    
#Create an HDF file
my $SDobj = PDL::IO::HDF::SD->new( "-$testfile" );
        
#Define some data
my $data = sequence(short, 500, 5);
my $square_data = sequence(short, 50, 50);

# TEST 1:
#Put data in file as 'myData' dataset
#with the names of dimensions ('dim1' and 'dim2')
ok( $SDobj->SDput("myData", $data , ['dim1','dim2']), 'SDput()' );

# TEST 2:
#Put some local attributs in 'myData'
#Set the fill value as 0
ok( $SDobj->SDsetfillvalue("myData", 0), 'SDsetfillvalue()' );

# TEST 3:
#Set the valid range from 0 to 2000
ok( $SDobj->SDsetrange("myData", [0, 2000]), 'SDsetrange()' );

# TEST 4:
#Set the default calibration for 'myData' (scale factor = 1, other = 0)
ok( $SDobj->SDsetcal("myData"), 'SDsetcal()' );

# TEST 5:
#Set a global text attribut
ok( $SDobj->SDsettextattr('This is a global text test!!', "myGText" ), 'SDsettextattr() (global)' );

# TEST 6:
#Set a local text attribut for 'myData'
ok( $SDobj->SDsettextattr('This is a local text testl!!', "myLText", "myData" ), 'SDsettextattr() (local)' );

# TEST 7:
#Set a global value attribut (you can put all values you want)
ok( $SDobj->SDsetvalueattr( PDL::short( 20 ), "myGValue"), 'SDSetvalueattr() (global)' );

# TEST 8:
#Set a local value attribut (you can put all values you want)
ok( $SDobj->SDsetvalueattr( PDL::long( [20, 15, 36] ), "myLValues", "myData" ), 'SDSetvalueattr() (local)' );

# TEST 9:
#Put square_data in file as 'mySquareData' dataset
#with the names of dimensions ('square_dim' and 'square_dim')
ok( $SDobj->SDput("mySquareData", $square_data , ['square_dim','square_dim']), 'SDput()' );

#Close the file
$SDobj->close;

# TEST 10:
# Test Hishdf:
ok( PDL::IO::HDF::SD::Hishdf( $testfile ), 'Hishdf()' );
    
### Reading from a HDF file

#Open an HDF file in read only mode
my $SDobj2 = PDL::IO::HDF::SD->new( $testfile );

# TEST 11:
#Get a list of all datasets
my @dataset_list = $SDobj2->SDgetvariablenames();
ok( $#dataset_list+1, 'SDgetvariablenames()' );

# TEST 12:
#Get a list of all global attributes name
my @globattr_list = $SDobj2->SDgetattributenames();
ok( $#globattr_list+1, 'SDgetattributenames() (global)' );

# TEST 13:
#Get a list of local attributes name for a dataset
my @locattr_list = $SDobj2->SDgetattributenames( "myData" );
#print "\@locattr_list = " . join(", ", @locattr_list ) . "\n";
ok( $#locattr_list+1, 'SDgetattributenames() (local)' );

# TEST 14:
#Get the value of local attribute for a dataset
my $value = $SDobj2->SDgetattribute( "myLText", "myData" );
ok( defined($value), 'SDgetattribute() (local)' );

# TEST 15:
#Get the all dataset 'myData'
$data = $SDobj2->SDget("myData");
ok( $data->nelem() > 0, 'SDget()' );
#print "info : ".$data->info."\n";

# TEST 16:
#Get dimension name of dataset 'myData'
my @dim = $SDobj2->SDgetdimnames("myData");
ok( ($dim[0] eq "dim1") && ($dim[1] eq "dim2") , 'SDgetdimnames()' );

# TEST 17:
#Get dimension size of dataset 'myData'
my @dim_square = $SDobj2->SDgetdimsize("myData");
ok( ($dim_square[0] == 5) && ($dim_square[1] == 500), 'SDgetdimsize()' );

# TEST 18:
#Get dimension name of dataset 'mySquareData'
@dim_square = $SDobj2->SDgetdimnames("mySquareData");
ok( ($dim_square[0] eq "square_dim") && ($dim_square[1] eq "square_dim"), 'SDgetdimnames()' );

# TEST 19:
# Get dimension size of dataset 'mySquareData'
@dim_square = $SDobj2->SDgetdimsize("mySquareData");
ok( ($dim_square[0] == 50) && ($dim_square[1] == 50), 'SDgetdimsize()' );

# TEST 20:
#Get the all dataset 'mySquareData'
my $square_data_get = $SDobj2->SDget("mySquareData");
ok( $square_data_get->nelem() > 0, 'SDget()' );
#print "info : ".$data->info."\n";


# TEST 21:
#Apply the scale factor of 'myData'
my $res = $SDobj2->SDgetscalefactor("myData");
ok( defined($res), 'SDgetscalefactor()' );

# TEST 22:
#Get the fill value
#The fill value corresponding to the BAD value in pdl
$res = $SDobj2->SDgetfillvalue("myData");
ok( defined($res), 'SDgetfillvalue()' );

# TEST 23:
#Get the valid range of datas
my @range = $SDobj2->SDgetrange("myData");
ok( $#range+1, 'SDgetrange()' );

#print Data::Dumper->Dump([$SDobj2],[qw(SDobj2)]);
 
#Now you can do what you want with your data
$SDobj2->close;

#
# These are from the old sdcompress.t test file:
#
undef($data);
my $HDFobj = PDL::IO::HDF::SD->new("-$testfile");

# TEST 24:
#Define some data
$data = ones( short, 5000, 5);
#Put data in file as 'myData' dataset
#with the names of dimensions ('dim1' and 'dim2')
ok( $HDFobj->SDput("myData", $data , ['dim1','dim2']), 'SDput()' );

# TEST 25:
$HDFobj->SDput("myData", $data , ['dim1','dim2']);
$data = $HDFobj->SDget("myData");
ok( $data->nelem(), 'SDget()' );

$HDFobj->close();

#
# These tests are from the old 11sdchunk.t test file:
#
my $hdf = PDL::IO::HDF::SD->new( "-$testfile" );

# TEST 26:
# Make sure chunking is on by default:
ok( $hdf->Chunking(), 'Chunking()' );

# Turn off chunking:
$hdf->Chunking(0);

# TEST 27:
# Make sure it's really off:
ok( !$hdf->Chunking(), 'Chunking(0)' );

# Write out a normal dataset:
my $dataset = sequence( byte, 10, 10 );
$res = $hdf->SDput( "NO_CHUNK", $dataset );

# TEST 28:
# Make sure we can write unchunked SDs:
ok( $res, 'SDput() (unchunked)' );

$hdf->close();
undef($hdf);

# TEST 29 & 30:
# Make sure we can read it properly:
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

# TEST 31:
# Make sure the chunked write works:
$res = $hdf->SDput( "CHUNK_2D", $dataset2d );
ok( $res, 'SDput() (chunked, 2D)' );

# TEST 32:
# Make sure it works with more than 2 dims:
my $dataset3d = sequence( long, 200, 200, 10 );
$res = $hdf->SDput( "CHUNK_3D", $dataset3d );
ok( $res, 'SDput() (chunked, 3D)');

$hdf->close();
undef($hdf);

# Verify the datasets we just wrote:
$hdf = PDL::IO::HDF::SD->new( $testfile );

# TEST 33 & 34:
my $dataset2d_test = $hdf->SDget( "CHUNK_2D" );
$good = $dataset2d_test->nelem() > 0;
ok( $good, 'SDget() (chunked, 2D)' );
$do_skip = $good ? '' : 'Skip if failed previous test!';
SKIP: {
    skip( "Previous test failed!", 1 ) if $do_skip;
    ok( tapprox( $dataset2d, $dataset2d_test ), 'comparing datasets written out and read in (chunked, 2D)' );
}

# TEST 35 & 36:
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

#
# These tests are from the old 07hdfdump.t test file:
#
my $H = PDL::IO::HDF->new( $testfile );

print ">>Global attributes :\n";
foreach my $attr ( $$H{SD}->SDgetattributenames() )
{
    my $curattr = $$H{SD}->SDgetattribute( $attr );
    print "\t$attr = \n";
    foreach ( split("\n", $curattr) )
        { print "\t\t$_\n"; }
}
    
print ">>Datasets :\n";
foreach my $dataset ( $$H{SD}->SDgetvariablenames() )
{
    print "\t$dataset\n";
    print "\t\tdimensions : \n";	
    my @dimname = $$H{SD}->SDgetdimname( $dataset );
    my @dimsize = $$H{SD}->SDgetdimsize( $dataset );
    my @dimsizeU = $$H{SD}->SDgetunlimiteddimsize( $dataset );
    foreach ( my $i = 0; $i <= $#dimsize; $i++ )
    {
        print "\t\t\t$dataset:$dimname[$i] = " 
            . ( (!$dimsize[$i]) ? "$dimsizeU[$i] (UNLIMITED)\n" : "$dimsize[$i]\n");
    }
        
    print "\t\tlocal attributes : \n";
    foreach my $locattr ( $$H{SD}->SDgetattributenames( $dataset ) )
    {
        my $curattr = $$H{SD}->SDgetattribute( $locattr, $dataset );
        print "\t\t\t$dataset:$locattr = $curattr\n";
    }
}
    
print ">>VData :\n";
foreach my $Vname ( $$H{VS}->VSgetnames() )
{
    if( !$$H{VS}->VSisattr($Vname) )
    {
        print "\t$Vname\n";
        foreach my $Vfieldname ( $$H{VS}->VSgetfieldnames( $Vname ) )
        {
            my $val = $$H{VS}->VSread( $Vname, $Vfieldname);
            print "\t\t$Vfieldname " 
                . ( ( $val->nelem > 10 ) ? ": too many values!\n" : "= $val\n" );
                
            print "**** $val\n"
                if( $Vfieldname eq 'attach_flag' );
        }
    }
}

$H->close();

# Remove the testfile:
unlink( $testfile );

exit(0);
