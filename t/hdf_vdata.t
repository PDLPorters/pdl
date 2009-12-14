#!/usr/bin/perl -w
#
# t/hdf_vdata.t
#
# Tests Vdata features of the HDF library.
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
        plan tests => 21;
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

use PDL::IO::HDF;
use PDL::IO::HDF::VS;

# Vdata test suite
use PDL::Config;
my $tmpdir = $PDL::Config{TEMPDIR};

my $testfile = "$tmpdir/vdata.hdf";

# creating

# TEST 1:
my $Hid = PDL::IO::HDF::VS::_Hopen( $testfile, PDL::IO::HDF->DFACC_CREATE, 2);
ok( $Hid != PDL::IO::HDF->FAIL );

PDL::IO::HDF::VS::_Vstart( $Hid );
my $vdata_id = PDL::IO::HDF::VS::_VSattach( $Hid, -1, "w" );
PDL::IO::HDF::VS::_VSsetname( $vdata_id, 'vdata_name' );
PDL::IO::HDF::VS::_VSsetclass( $vdata_id, 'vdata_class' );

# TEST 2:
my $vdata_ref = PDL::IO::HDF::VS::_VSgetid( $Hid, -1 );
ok( $vdata_ref != PDL::IO::HDF->FAIL );

# TEST 3:
my $name = "";
PDL::IO::HDF::VS::_VSgetname( $vdata_id, $name );
ok( $name eq "vdata_name" );

# TEST 4:
my $class = "";
PDL::IO::HDF::VS::_VSgetclass( $vdata_id, $class );
ok( $class eq "vdata_class" );

my $data = PDL::float sequence(10);
my $HDFtype = $PDL::IO::HDF::SDtypeTMAP->{$data->get_datatype()};

# TEST 5:
ok( PDL::IO::HDF::VS::_VSfdefine( $vdata_id, 'PX', $HDFtype, 1) );

# TEST 6:
ok( PDL::IO::HDF::VS::_VSsetfields( $vdata_id, 'PX') );

# TEST 7:
ok( PDL::IO::HDF::VS::_VSwrite( $vdata_id, $data, 10, PDL::IO::HDF->FULL_INTERLACE ) );

PDL::IO::HDF::VS::_VSdetach( $vdata_id );
PDL::IO::HDF::VS::_Vend( $Hid );

# TEST 8:
ok( PDL::IO::HDF::VS::_Hclose( $Hid ) );

# TEST 9:
undef( $Hid );
$Hid = PDL::IO::HDF::VS::_Hopen( $testfile, PDL::IO::HDF->DFACC_READ, 2 );
ok( $Hid != PDL::IO::HDF->FAIL );

PDL::IO::HDF::VS::_Vstart( $Hid );

# TEST 10:
$vdata_ref = PDL::IO::HDF::VS::_VSfind( $Hid, 'vdata_name' );
ok( $vdata_ref != PDL::IO::HDF->FAIL );

# TEST 11:
$vdata_id = PDL::IO::HDF::VS::_VSattach( $Hid, $vdata_ref, "r" );
ok( $vdata_id != PDL::IO::HDF->FAIL );

# TEST 12:
my $vdata_size = 0;
my $n_records = 0;
my $interlace = 0;
my $fields = "";
my $vdata_name = "";
ok( PDL::IO::HDF::VS::_VSinquire( $vdata_id, $n_records, $interlace, $fields, $vdata_size, $vdata_name) );

# TEST 13:
my @tfields = split(",",$fields);
my $data_type = PDL::IO::HDF::VS::_VFfieldtype( $vdata_id, 0 );
$data = ones( $PDL::IO::HDF::SDinvtypeTMAP2->{$data_type}, 10 );
ok( PDL::IO::HDF::VS::_VSread( $vdata_id, $data, $n_records, $interlace ) );

# TEST 14:
my $expected_data = sequence(10);
ok( sub { tapprox( $data, $expected_data ) } );

PDL::IO::HDF::VS::_VSdetach( $vdata_id );
PDL::IO::HDF::VS::_Vend( $Hid );

# TEST 15:
ok( PDL::IO::HDF::VS::_Hclose( $Hid ) );

# TEST 16:
my $vdataOBJ = PDL::IO::HDF::VS->new( $testfile );
ok( defined( $vdataOBJ ) );

# TEST 17:
my @vnames = $vdataOBJ->VSgetnames();
ok( scalar( @vnames ) > 0 );

foreach my $name ( @vnames ) 
{
    # TEST 18:
    my @fields = $vdataOBJ->VSgetfieldnames( $name );
    ok( scalar( @fields ) > 0 );    
    
    foreach my $field ( @fields ) 
    {
        # TEST 19:
        my $data = $vdataOBJ->VSread( $name, $field );
        ok( defined( $data ) );
    }
}

# TEST 20:
ok( $vdataOBJ->close() );
undef( $vdataOBJ );

# TEST 21:
$vdataOBJ=PDL::IO::HDF::VS->new( $testfile );
foreach my $name ( $vdataOBJ->VSgetnames() ) 
{ 
    print "name: $name\n";
    foreach my $field ( $vdataOBJ->VSgetfieldsnames( $name ) ) 
    {
        print "   $field\n";
        my $data = $vdataOBJ->VSread( $name, $field );
        print "     " . $data->info() . "\n";
        print "        $data\n";
    }
}
ok( 1 );

# Remove the testfile:
unlink( $testfile );

exit(0);

