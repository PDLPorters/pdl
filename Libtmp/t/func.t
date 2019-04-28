use strict;
use warnings;
use Test::More;
use PDL::LiteF;

# Must load slatec before Func since Func loads slatec itself
# and this line will be a no-op (and so we will not be able to
# spot that Slatec has failed)
my $slatec;
BEGIN { eval "use PDL::Slatec"; $slatec = ($@ ? 0 : 1); }

use PDL::Func;

##########################################################

my $x = float( 1, 2, 3, 4, 5, 6, 8, 10 );
my $y = ($x * 3) * ($x - 2);

my $obj = PDL::Func->init( x => $x, y => $y );
is( $obj->scheme() , 'Linear', 'default scheme is linear' );  # 1

my $xi = $x - 0.5;
my $yi = $obj->interpolate( $xi );
is( $obj->status, -1, 'non serious error from interpolate: extrapolation used' );

# compare to direct version
my ( $ans, $err ) = PDL::Primitive::interpolate( $xi, $x, $y );
my $d = abs( $ans - $yi ); 
ok( all($d < 1.0e-5), 'compare to PDL::Primitive::interpolate');

my $oerr = $obj->get( 'err' );
ok( all ($oerr-$err) == 0 , 'no error after interpolation');

# check we trap a call to an unavailable method
eval { $obj->gradient( $xi ); };
isnt( $@ , '' ,'calling unavailable method'); # 5

unless ($slatec) {
  done_testing;
  exit;
}

$x = sequence(float,10);
$y = $x*$x + 0.5;
#$obj->set( Interpolate => "Hermite", x => $x, y => $y, bc => "simple" );
$obj->set( Interpolate => 'Hermite', x => $x, y => $y );

#print "bc for Hermite interpolation: " . $obj->get('bc') . "\n";
is( $obj->scheme() , 'Hermite' , 'scheme is Hermite'); 
is( $obj->get('bc'), 'simple' , 'boundary condition is simple'); 
is( $obj->status, 1 , 'no errors');

my $gi;

$xi = sequence(float,5) + 2.3;
$yi = $obj->interpolate( $xi );
is( $obj->status, 1, 'status==1 after interpolate');

$ans = $xi*$xi + 0.5;
$d   = abs( $ans - $yi );
ok( all($d <= 0.03), 'interpolate correct answer');

$gi = $obj->gradient( $xi );
is( $obj->status, 1, 'status==1 after gradient');

$ans = 2*$xi;
$d   = abs( $ans - $gi );
ok( all($d <= 0.04), 'gradient correct answer');

# see how they cope with threading 
#
$y = cat( $x*$x+43.3, $x*$x*$x-23 );

$obj->set( x => $x, y => $y );
is( $obj->status , 1, 'threading: status==1 after set');

$yi = $obj->interpolate( $xi );
is( $obj->status, 1 ,'threading: status==1 after interpolate');
ok( ( (dims($yi) == 2) & ($yi->getdim(0) == $xi->getdim(0))) & ($yi->getdim(1) == 2), 'threading dimension check' );

$ans = cat( $xi*$xi+43.3, $xi*$xi*$xi-23 );
$d   = abs( $ans - $yi );
ok( all($d <= 6), 'threading: correct answer' );

done_testing;
