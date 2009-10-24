#!/usr/bin/perl -w
#
# t/hdf_sd.t
#
# Tests the SD interface to the HDF library.
#
# 29 March 2006
# Judd Taylor, USF IMaRS
#
use strict;
use PDL;
use Test::More;

BEGIN
{
    eval( " use PDL::IO::HDF; " );
    if( $@ )
    {
        plan skip_all => "PDL::IO::HDF module not available.";
    }  
    else
    {
        plan tests => 32;
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

# TEST 1:
#Put data in file as 'myData' dataset
#with the names of dimensions ('dim1' and 'dim2')
ok( $SDobj->SDput("myData", $data , ['dim1','dim2']) );

# TEST 2:
#Put some local attributs in 'myData'
#Set the fill value as 0
ok( $SDobj->SDsetfillvalue("myData", 0) );

# TEST 3:
#Set the valid range from 0 to 2000
ok( $SDobj->SDsetrange("myData", [0, 2000]) );

# TEST 4:
#Set the default calibration for 'myData' (scale factor = 1, other = 0)
ok( $SDobj->SDsetcal("myData") );

# TEST 5:
#Set a global text attribut
ok( $SDobj->SDsettextattr('This is a global text test!!', "myGText" ) );

# TEST 6:
#Set a local text attribut for 'myData'
ok( $SDobj->SDsettextattr('This is a local text testl!!', "myLText", "myData" ) );

# TEST 7:
#Set a global value attribut (you can put all values you want)
ok( $SDobj->SDsetvalueattr( PDL::short( 20 ), "myGValue") );

# TEST 8:
#Set a local value attribut (you can put all values you want)
ok( $SDobj->SDsetvalueattr( PDL::long( [20, 15, 36] ), "myLValues", "myData" ) );

#Close the file
$SDobj->close;

# TEST 9:
# Test Hishdf:
ok( PDL::IO::HDF::SD::Hishdf( $testfile ) );
    
### Reading from a HDF file

#Open an HDF file in read only mode
my $SDobj2 = PDL::IO::HDF::SD->new( $testfile );

# TEST 10:
#Get a list of all datasets
my @dataset_list = $SDobj2->SDgetvariablenames();
ok( $#dataset_list+1 );

# TEST 11:
#Get a list of all global attributes name
my @globattr_list = $SDobj2->SDgetattributenames();
ok( $#globattr_list+1 );

# TEST 12:
#Get a list of local attributes name for a dataset
my @locattr_list = $SDobj2->SDgetattributenames( "myData" );
#print "\@locattr_list = " . join(", ", @locattr_list ) . "\n";
ok( $#locattr_list+1 );

# TEST 13:
#Get the value of local attribute for a dataset
my $value = $SDobj2->SDgetattribute( "myLText", "myData" );
ok( defined($value) );

# TEST 14:
#Get the all dataset 'myData'
$data = $SDobj2->SDget("myData");
ok( $data->nelem() > 0 );
#print "info : ".$data->info."\n";

# TEST 15:
#Apply the scale factor of 'myData'
my $res = $SDobj2->SDgetscalefactor("myData");
ok( defined($res) );

# TEST 16:
#Get the fill value
#The fill value corresponding to the BAD value in pdl
$res = $SDobj2->SDgetfillvalue("myData");
ok( defined($res) );

# TEST 17:
#Get the valid range of datas
my @range = $SDobj2->SDgetrange("myData");
ok( $#range+1 );

#print Data::Dumper->Dump([$SDobj2],[qw(SDobj2)]);
 
#Now you can do what you want with your data
$SDobj2->close;

#
# These are from the old sdcompress.t test file:
#
undef($data);
my $HDFobj = PDL::IO::HDF::SD->new("-$testfile");

# TEST 18:
#Define some data
$data = ones( short, 5000, 5);
#Put data in file as 'myData' dataset
#with the names of dimensions ('dim1' and 'dim2')
ok( $HDFobj->SDput("myData", $data , ['dim1','dim2']) );

# TEST 19:
# Compress the SD dataset
# No longer necessary with chunking on by default:
#$res = $HDFobj->SDsetcompress("myData", 5);
ok( 1 );

# TEST 20:
$HDFobj->SDput("myData", $data , ['dim1','dim2']);
$data = $HDFobj->SDget("myData");
ok( $data->nelem() );

$HDFobj->close();

#
# These tests are from the old 11sdchunk.t test file:
#
my $hdf = PDL::IO::HDF::SD->new( "-$testfile" );

# TEST 21:
# Make sure chunking is on by default:
ok( $hdf->Chunking() );

# Turn off chunking:
$hdf->Chunking(0);

# TEST 22:
# Make sure it's really off:
ok( !$hdf->Chunking() );

# Write out a normal dataset:
my $dataset = sequence( byte, 10, 10 );
$res = $hdf->SDput( "NO_CHUNK", $dataset );

# TEST 23:
# Make sure we can write unchunked SDs:
ok( $res );

$hdf->close();
undef($hdf);

# TEST 24 & 25:
# Make sure we can read it properly:
$hdf = PDL::IO::HDF::SD->new( $testfile );

my $dataset_test = $hdf->SDget( "NO_CHUNK" );
my $good = ($dataset_test->nelem() > 0) ? 1 : 0;
ok( $good );
my $do_skip = $good ? '' : 'Skip if failed previous test!';
SKIP: {
    skip( "Previous test failed!", 1 ) if $do_skip;
    ok( tapprox( $dataset, $dataset_test ) );
}

$hdf->close();
undef($hdf);
unlink( $testfile );

# Reopen to write out the chunked portion:
$hdf = PDL::IO::HDF::SD->new( "-$testfile" );

my $dataset2d = sequence( long, 200, 200 );

# TEST 26:
# Make sure the chunked write works:
$res = $hdf->SDput( "CHUNK_2D", $dataset2d );
ok( $res );

# TEST 27:
# Make sure it works with more than 2 dims:
my $dataset3d = sequence( long, 200, 200, 10 );
$res = $hdf->SDput( "CHUNK_3D", $dataset3d );
ok( $res );

$hdf->close();
undef($hdf);

# Verify the datasets we just wrote:
$hdf = PDL::IO::HDF::SD->new( $testfile );

# TEST 28 & 29:
my $dataset2d_test = $hdf->SDget( "CHUNK_2D" );
$good = $dataset2d_test->nelem() > 0;
ok( $good );
$do_skip = $good ? '' : 'Skip if failed previous test!';
SKIP: {
    skip( "Previous test failed!", 1 ) if $do_skip;
    ok( tapprox( $dataset2d, $dataset2d_test ) );
}

# TEST 30 & 31:
my $dataset3d_test = $hdf->SDget( "CHUNK_3D" );
$good = $dataset3d_test->nelem() > 0;
ok( $good );
$do_skip = $good ? '' : 'Skip if failed previous test!';
SKIP: {
    skip( "Previous test failed!", 1 ) if $do_skip;
    ok( tapprox( $dataset3d, $dataset3d_test ) );
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

# TEST 32:
$H->close();
ok( 1 );


# Remove the testfile:
unlink( $testfile );

exit(0);
