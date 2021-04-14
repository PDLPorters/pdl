# Tests Vdata features of the HDF library.
#
# 29 March 2006
# Judd Taylor, USF IMaRS
#
use strict;
use PDL;
use Test::More;
use PDL::IO::HDF::VS;
use File::Temp qw(tempdir);

sub tapprox {
    my $x = shift;
    my $y = shift;
    my $d = abs($x - $y);
    #ok( all($d < 1.0e-5) );
    return all($d < 1.0e-5);
}

# Vdata test suite
my $tmpdir = tempdir( CLEANUP => 1 );
my $testfile = "$tmpdir/vdata.hdf";

# creating

my $Hid = PDL::IO::HDF::VS::_Hopen( $testfile, PDL::IO::HDF->DFACC_CREATE, 2);
ok( $Hid != PDL::IO::HDF->FAIL );

PDL::IO::HDF::VS::_Vstart( $Hid );
my $vdata_id = PDL::IO::HDF::VS::_VSattach( $Hid, -1, "w" );
PDL::IO::HDF::VS::_VSsetname( $vdata_id, 'vdata_name' );
PDL::IO::HDF::VS::_VSsetclass( $vdata_id, 'vdata_class' );

my $vdata_ref = PDL::IO::HDF::VS::_VSgetid( $Hid, -1 );
ok( $vdata_ref != PDL::IO::HDF->FAIL );

my $name = "";
PDL::IO::HDF::VS::_VSgetname( $vdata_id, $name );
ok( $name eq "vdata_name" );

my $class = "";
PDL::IO::HDF::VS::_VSgetclass( $vdata_id, $class );
ok( $class eq "vdata_class" );

my $data = PDL::float sequence(10);
my $HDFtype = $PDL::IO::HDF::SDtypeTMAP->{$data->get_datatype()};

ok( PDL::IO::HDF::VS::_VSfdefine( $vdata_id, 'PX', $HDFtype, 1) );
ok( PDL::IO::HDF::VS::_VSsetfields( $vdata_id, 'PX') );
ok( PDL::IO::HDF::VS::_VSwrite( $vdata_id, $data, 10, PDL::IO::HDF->FULL_INTERLACE ) );

PDL::IO::HDF::VS::_VSdetach( $vdata_id );
PDL::IO::HDF::VS::_Vend( $Hid );

ok( PDL::IO::HDF::VS::_Hclose( $Hid ) );

undef( $Hid );
$Hid = PDL::IO::HDF::VS::_Hopen( $testfile, PDL::IO::HDF->DFACC_READ, 2 );
ok( $Hid != PDL::IO::HDF->FAIL );

PDL::IO::HDF::VS::_Vstart( $Hid );

$vdata_ref = PDL::IO::HDF::VS::_VSfind( $Hid, 'vdata_name' );
ok( $vdata_ref != PDL::IO::HDF->FAIL );

$vdata_id = PDL::IO::HDF::VS::_VSattach( $Hid, $vdata_ref, "r" );
ok( $vdata_id != PDL::IO::HDF->FAIL );

my $vdata_size = 0;
my $n_records = 0;
my $interlace = 0;
my $fields = "";
my $vdata_name = "";
ok( PDL::IO::HDF::VS::_VSinquire( $vdata_id, $n_records, $interlace, $fields, $vdata_size, $vdata_name) );

my @tfields = split(",",$fields);
my $data_type = PDL::IO::HDF::VS::_VFfieldtype( $vdata_id, 0 );
$data = ones( $PDL::IO::HDF::SDinvtypeTMAP2->{$data_type}, 10 );
ok( PDL::IO::HDF::VS::_VSread( $vdata_id, $data, $n_records, $interlace ) );

my $expected_data = sequence(10);
ok( sub { tapprox( $data, $expected_data ) } );

PDL::IO::HDF::VS::_VSdetach( $vdata_id );
PDL::IO::HDF::VS::_Vend( $Hid );

ok( PDL::IO::HDF::VS::_Hclose( $Hid ) );

my $vdataOBJ = PDL::IO::HDF::VS->new( $testfile );
ok( defined( $vdataOBJ ) );

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

ok( $vdataOBJ->close() );
undef( $vdataOBJ );

done_testing;
