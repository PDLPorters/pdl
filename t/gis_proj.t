#!/usr/bin/perl
no warnings qw(misc);

#
# t/gis_proj.t
#
# Test program for the PDL::GIS::Proj library
#
# Judd Taylor, Orbital Systems, Ltd.
# 18 March 2003
#

use strict;
use PDL;
use Test::More;

BEGIN
{
    use PDL::Config;
    if ( $PDL::Config{WITH_PROJ} ) 
    {
        eval( " use PDL::GIS::Proj; " );
        if( $@ )
        {
            plan skip_all => "PDL::GIS::Proj compiled, but not available.";
        }  
        else
        {
            plan tests => 15;
        }
    }
    else
    {
        plan skip_all => "PDL::GIS::Proj module not compiled.";
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

use PDL::GIS::Proj;

print "Testing forward transformation...\n";
my $proj = "+proj=merc +ellps=WGS72 +lon_0=80.25w +lat_0=30n";
print "Perl level params: \'$proj\'\n";

my $lon = double [-90.0, -95.0, -86.0];
my $lat = double [  0.0,  33.0,  77.0];
# Expected results:
my $x_exp = double [-1085364.69489521, -1641961.97432865, -640086.87134846];
my $y_exp = double [7.0811523e-10, 3872032.73513601, 13812394.85701733];

# TEST 1 & 2:
my ($x, $y) = fwd_transform($lon, $lat, $proj);
print "Inputs:\n\t\$lon = $lon\n\t\$lat = $lat\n";
print "Result:\n\t\$x = $x\n\t\$y = $y\n";
#print_hi_prec( "x", $x );
#print_hi_prec( "y", $y );
ok( sub { tapprox( $x, $x_exp ) } );
ok( sub { tapprox( $y, $y_exp ) } );

# TEST 3 & 4:
print "\nTesting inverse transformation...\n";
print "Perl level params: \'$proj\'\n";
my ($lon2, $lat2) = inv_transform($x, $y, $proj);
print "Inputs:\n\t\$x = $x\n\t\$y = $y\n";
print "Results:\n\t\$lon2 = $lon2\n\t\$lat2 = $lat2\n";
ok( sub { tapprox( $lon2, $lon ) } );
ok( sub { tapprox( $lat2, $lat ) } );

# Do the corners of a cyl eq map, and see what we get...
print "\nCorners of a cylindrical equidistant projection:\n";
my $cyl_eq = "+proj=eqc +lon_0=0";
print "Perl level params: \'$cyl_eq\'\n";
my $lon3 = double [-180.0, -180.0,  180.0,  180.0];
my $lat3 = double [  90.0,  -90.0,   90.0,  -90.0];
# Expexted results:
my $x3_exp = double [ -20037508.34278924,  -20037508.34278924,  20037508.34278924,  20037508.34278924 ];
my $y3_exp = double [ 10018754.17139462,  -10018754.17139462,  10018754.17139462,  -10018754.17139462 ];

# TEST 5 & 6:
my ($x3, $y3) = fwd_transform($lon3, $lat3, $cyl_eq);
print "Inputs:\n\t\$lon3 = $lon3\n\t\$lat3 = $lat3\n";
print "Result:\n\t\$x3 = $x3\n\t\$y3 = $y3\n";
ok( sub { tapprox( $x3, $x3_exp ) } );
ok( sub { tapprox( $y3, $y3_exp ) } );

#print_hi_prec( "x3", $x3 );
#print_hi_prec( "y3", $y3 );

($lat, $lon) = undef;

# TEST 7 & 8:
$lon = float [-90.0, -95.0, -86.0];
$lat = float [  0.0,  33.0,  77.0];

# Convert the previous expexted results to float:
my $tmp = $x_exp->sever();
$x_exp = float( $tmp );
$tmp = $y_exp->sever();
$y_exp = float( $tmp );

print "\nTesting inplace operation...\nForward:\n";
#my $format = "\tType: %T\n\tDim: %-15D\n\tState: %S\n\tFlow: %F\n\tClass: %C\n\tAddress: %A\n\tMem: %M\n";
print "Inputs:\n\t\$lon = $lon\n\t\$lat = $lat\n";
#print "\$lon INFO:\n" . $lon->info($format) . "\n";
#print "\$lat INFO:\n" . $lat->info($format) . "\n";

fwd_trans_inplace($lon, $lat, $proj);
print "Result:\n\t\$lon = $lon\n\t\$lat = $lat\n";
#print "\$lon INFO:\n" . $lon->info($format) . "\n";
#print "\$lat INFO:\n" . $lat->info($format) . "\n";
#print_hi_prec("lon", $lon);
#print_hi_prec("lat", $lat);
ok( sub { tapprox( $lon, $x_exp ) } );
ok( sub { tapprox( $lat, $y_exp ) } );

# TEST 9 & 10:
print "\nInverse:\n";
print "Inputs:\n\t\$lon = $lon\n\t\$lat = $lat\n";
# Expexted results:
my $lon_exp = float [-90.0, -95.0, -86.0];
my $lat_exp = float [  0.0,  33.0,  77.0];

inv_trans_inplace($lon, $lat, $proj);
print "Result:\n\t\$lon = $lon\n\t\$lat = $lat\n";
ok( sub { tapprox( $lon, $lon_exp ) } );
ok( sub { tapprox( $lat, $lat_exp ) } );

# TEST 11:
print "\nTesting get_proj_info()...\n";
print "PROJ INFO: \n" . get_proj_info($proj) . "\n";
ok( 1 );

# TEST 12 & 13:

print "\nTesting 2d inplace operation...\n";
$lat = (yvals( double, 35, 17 ) - 8.0) * 10.0;
$lon = (xvals( double, 35, 17 ) - 17.0) * 10.0;
print "Inputs:\n\t\$lon = $lon\n\t\$lat = $lat\n";
fwd_trans_inplace($lon, $lat, $proj);
print "Result:\n\t\$lon = $lon\n\t\$lat = $lat\n";

ok(1);
ok(1);

# TEST 14: 
# Get projection descriptions:
my $projections = load_projection_descriptions();
#use Data::Dumper;
#my $dd = Data::Dumper->new( [$projections], ['projections'] );
#$dd->Indent(1);
#print STDERR $dd->Dump();
ok(1);

# TEST 15:
# Get full projection information:
my $info = load_projection_information();
#use Data::Dumper;
#foreach ( sort keys %$info )
#{
#    my $dd2 = Data::Dumper->new( [ $info->{$_} ], [ $_ ] );
#    $dd2->Indent(1);
#    print STDERR $dd2->Dump() . "\n";
#}
ok(1);


exit(0);

sub print_hi_prec
{
    my $name = shift;
    my $pdl = shift;
    my $type = $pdl->type();
    my $last = $pdl->dim(0) - 1;
    print "\$$name = $type [ ";
    foreach my $i ( 0 .. $last - 1 )
        { printf("%.8f, ", $pdl->at($i) ); }
    printf("%.8f ];\n", $pdl->at( $last ));
    return;
}
