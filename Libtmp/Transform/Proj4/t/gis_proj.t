use strict;
use warnings;
use PDL::LiteF;
use Test::More;
BEGIN {
diag "ENV $_ = ", explain $ENV{$_}
  for qw(LD_LIBRARY_PATH DYLD_LIBRARY_PATH LDFLAGS CFLAGS CXXFLAGS LD_RUN_PATH
    LC_RUN_PATH);
}
use PDL::Transform::Proj4;
use Alien::proj;
diag "Alien::proj version $Alien::proj::VERSION";

sub tapprox
{
    my $x = shift;
    my $y = shift;
    my $d = abs($x - $y);
    my $res = all($d < 1.0e-5);
    diag "got:$x\nexpected:$y" if !$res;
    $res;
}

my @version = eval { PDL::Transform::Proj4::proj_version() };
is $@, '', 'proj_version no die';
diag "PROJ version: @version";

print "Testing forward transformation...\n";
my $proj = "+proj=merc +ellps=WGS72 +lon_0=80.25w +lat_0=30n";
print "Perl level params: \'$proj\'\n";

my $lonlat = double [[-90.0,0.0], [-95.0,33.0], [-86.0,77.0]];
# Expected results:
my $xy_exp = double [
  [-1085364.69489521, 7.0811523e-10],
  [-1641961.97432865, 3872032.73513601],
  [-640086.87134846, 13812394.85701733],
];

my ($xy) = PDL::Transform::Proj4::fwd_transform($lonlat, $proj);
ok( tapprox( $xy, $xy_exp ) );

my ($lonlat2) = PDL::Transform::Proj4::inv_transform($xy, $proj);
ok( tapprox( $lonlat2, $lonlat ) );

# Do the corners of a cyl eq map, and see what we get...
my $cyl_eq = "+proj=eqc +lon_0=0 +datum=WGS84";
my $lonlat3 = double [[-180.0,90.0], [-180.0,-90.0], [180.0,90.0], [180.0,-90.0]];
my $xy3_exp = double [
  [-20037508.34278924, 10018754.17139462],
  [-20037508.34278924, -10018754.17139462],
  [20037508.34278924, 10018754.17139462],
  [20037508.34278924, -10018754.17139462]
];

my ($xy3) = PDL::Transform::Proj4::fwd_transform($lonlat3, $cyl_eq);
ok( tapprox( $xy3, $xy3_exp ) );

$lonlat = ((xvals( double, 35, 17 ) - 17.0) * 10.0)->cat(
  (yvals( double, 35, 17 ) - 8.0) * 10.0)->mv(2,0);
my $exp = $lonlat->copy;
$lonlat->inplace(1);
PDL::Transform::Proj4::fwd_transform($lonlat, $proj);
ok !all(approx($lonlat, $exp)), 'check it changed';

# Get full projection information:
my $info = PDL::Transform::Proj4::load_projection_information();
#diag explain $info;

my @units = PDL::Transform::Proj4::units($cyl_eq);
is 0+@units, 2, 'got 2 units back';

done_testing;
