use strict;
use warnings;
use Test::More;
use Test::PDL;
use PDL::LiteF;
use PDL::Func qw(pchip spline);

my $x = float( 1, 2, 3, 4, 5, 6, 8, 10 );
my $y = ($x * 3) * ($x - 2);
my $xi = $x - 0.5;
my $obj = PDL::Func->init( x => $x, y => $y );
is( $obj->scheme() , 'Linear', 'default scheme is linear' );  # 1
is_pdl $obj->interpolate( $xi ), pdl('-4.5 -1.5 4.5 16.5 34.5 58.5 126 216'), {atol=>1e-5, test_name=>'linear interpolate'};
is $obj->status, -1, 'non serious error from linear interpolate: extrapolation used';
is_pdl $obj->get( 'err' ), long('1 0 0 0 0 0 0 0'), 'same error as direct';

eval { $obj->gradient( $xi ); };
like $@ , qr/can not call gradient/, 'calling unavailable method';

$x = sequence(float,10);
$y = $x*$x + 0.5;
$obj->set( Interpolate => 'Hermite', x => $x, y => $y );
is( $obj->scheme() , 'Hermite' , 'scheme is Hermite'); 
is( $obj->get('bc'), 'simple' , 'boundary condition is simple'); 
is( $obj->status, 1 , 'no errors');

$xi = sequence(float,5) + 2.3;
is_pdl $obj->interpolate( $xi ), $xi*$xi + 0.5, {atol=>0.04, test_name=>'interpolate'};
is( $obj->status, 1, 'status==1 after interpolate');

is_pdl scalar $obj->gradient( $xi ), 2*$xi, {atol=>0.04, test_name=>'gradient'};
is( $obj->status, 1, 'status==1 after gradient');

# see how they cope with broadcasting
$y = cat( $x*$x+43.3, $x*$x*$x-23 );
$obj->set( x => $x, y => $y );
is( $obj->status , 1, 'broadcasting: status==1 after set');
my $ans = cat( $xi*$xi+43.3, $xi*$xi*$xi-23 );
is_pdl $obj->interpolate( $xi ), $ans, {atol=>6, test_name=>'broadcasting'};
is( $obj->status, 1 ,'broadcasting: status==1 after interpolate');

# non-simple boundary conditions
$obj->set( bc => {} );
is_pdl $obj->interpolate( $xi ), $ans, {atol=>6, test_name=>'broadcasting non-simple'};

is_pdl pchip( $x, $y, $xi ), $ans, {atol=>6, test_name=>'pchip'};
is_pdl spline( $x, $y, $xi ), $ans, {atol=>6, test_name=>'spline'};

done_testing;
