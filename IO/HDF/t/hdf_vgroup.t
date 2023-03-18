# Tests Vgroup features of the HDF library.
#
# 29 March 2006
# Judd Taylor, USF IMaRS
#
use strict;
use warnings;
use PDL;
use Test::More;
use PDL::IO::HDF::VS;
use File::Temp qw(tempdir);

my $tmpdir = tempdir( CLEANUP => 1 );
my $testfile = "$tmpdir/vgroup.hdf";

# Vgroup test suite
my $Hid = PDL::IO::HDF::VS::_Hopen( $testfile, PDL::IO::HDF->DFACC_CREATE, 2 );
ok( $Hid != -1 );

PDL::IO::HDF::VS::_Vstart( $Hid );

my $vgroup_id = PDL::IO::HDF::VS::_Vattach( $Hid, -1, "w" );
PDL::IO::HDF::VS::_Vsetname( $vgroup_id, 'vgroup_name' );
PDL::IO::HDF::VS::_Vsetclass( $vgroup_id, 'vgroup_class' );

my $vgroup_ref = PDL::IO::HDF::VS::_Vgetid( $Hid, -1 );
ok( $vgroup_ref != PDL::IO::HDF->FAIL );

is( PDL::IO::HDF::VS::_Vgetname($vgroup_id), "vgroup_name" );
is( PDL::IO::HDF::VS::_Vgetclass( $vgroup_id ), "vgroup_class" );

PDL::IO::HDF::VS::_Vdetach( $vgroup_id );

PDL::IO::HDF::VS::_Vend( $Hid );

ok( PDL::IO::HDF::VS::_Hclose( $Hid ) );

my $vOBJ = PDL::IO::HDF::VS->new( "+$testfile" );
ok( defined($vOBJ) );

ok( $vOBJ->Vcreate('10vgroup','vgroup_class2','vgroup_name') );

my @mains = $vOBJ->Vgetmains();
ok( scalar( @mains ) > 0 );

foreach my $Vmain ( @mains ) {
    my @Vchildren = $vOBJ->Vgetchildren( $Vmain );
    ok( scalar( @Vchildren ) > 0 );    
}

ok( $vOBJ->close() );

done_testing;
